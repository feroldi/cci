#include "cci/syntax/source_map.hpp"
#include "gtest/gtest.h"

using namespace cci::syntax;

namespace
{

struct SourceMapTest : ::testing::Test
{
protected:
    SourceMap source_map;
    SourceMap mbc_source_map;
    const FileMap *filemap1;
    const FileMap *filemap2;
    const FileMap *filemap3;

    SourceMapTest() : source_map()
    {
        filemap1 = &source_map.create_owned_filemap("block1.c",
                                                    "first line\n"); // (0, 11)
        filemap2 = &source_map.create_owned_filemap("empty.c", ""); // (12, 12)
        filemap3 = &source_map.create_owned_filemap(
            "block2.c",
            "first line\nsecond line\nthird line\n"); // (13, 47)

        // Character sizes in bytes:
        //
        // 2  2  2  2  2  2  1  2  2  2  2  2  2  1
        // п  е  р  в  а  я     с  т  р  о  к  а  .
        //
        //  1  2  2  2  2  2  2  1  2  2  2  2  2  2  1  1.
        // \n  в  т  о  р  а  я     с  т  р  о  к  а  . \n.
        mbc_source_map.create_owned_filemap("mbc.c",
                                            "первая строка.\nвторая строка.\n");
    }
};

TEST_F(SourceMapTest, lookupFileMapIndex)
{
    EXPECT_EQ(0, source_map.lookup_filemap_idx(ByteLoc(0)));
    EXPECT_EQ(1, source_map.lookup_filemap_idx(ByteLoc(12)));
    EXPECT_EQ(2, source_map.lookup_filemap_idx(ByteLoc(13)));

    EXPECT_THROW(source_map.lookup_filemap_idx(ByteLoc(500)),
                 cci::broken_contract);
}

TEST_F(SourceMapTest, lookupLine)
{
    const auto [fm1, line1] = source_map.lookup_line(ByteLoc(10));
    EXPECT_EQ("block1.c", fm1.name);
    EXPECT_EQ(0, line1);

    const auto [fm2, line2] = source_map.lookup_line(ByteLoc(12));
    EXPECT_EQ("empty.c", fm2.name);
    EXPECT_EQ(0, line2);

    const auto [fm3, line3] = source_map.lookup_line(ByteLoc(24));
    EXPECT_EQ("block2.c", fm3.name);
    EXPECT_EQ(1, line3);

    const auto [fm4, line4] = source_map.lookup_line(ByteLoc(36));
    EXPECT_EQ("block2.c", fm4.name);
    EXPECT_EQ(2, line4);
}

TEST_F(SourceMapTest, lookupByteOffset)
{
    const auto [fm1, offset1] = source_map.lookup_byte_offset(ByteLoc(0));
    EXPECT_EQ("block1.c", fm1.name);
    EXPECT_EQ(ByteLoc(0), offset1);

    const auto [fm2, offset2] = source_map.lookup_byte_offset(ByteLoc(12));
    EXPECT_EQ("empty.c", fm2.name);
    EXPECT_EQ(ByteLoc(0), offset2);

    const auto [fm3, offset3] = source_map.lookup_byte_offset(ByteLoc(15));
    EXPECT_EQ("block2.c", fm3.name);
    EXPECT_EQ(ByteLoc(2), offset3);
}

TEST_F(SourceMapTest, lookupSourceLocation)
{
    const auto loc1 = source_map.lookup_source_location(ByteLoc(0));
    EXPECT_EQ("block1.c", loc1.file.name);
    EXPECT_EQ(1, loc1.line);
    EXPECT_EQ(CharPos(0), loc1.column);

    const auto loc2 = source_map.lookup_source_location(ByteLoc(28));
    EXPECT_EQ("block2.c", loc2.file.name);
    EXPECT_EQ(2, loc2.line);
    EXPECT_EQ(CharPos(4), loc2.column);
}

TEST_F(SourceMapTest, rangeToSnippet)
{
    EXPECT_EQ("first line",
              source_map.span_to_snippet(ByteSpan(ByteLoc(0), ByteLoc(10))));
    EXPECT_EQ("",
              source_map.span_to_snippet(ByteSpan(ByteLoc(12), ByteLoc(12))));
    EXPECT_EQ("first line",
              source_map.span_to_snippet(ByteSpan(ByteLoc(13), ByteLoc(23))));
    EXPECT_EQ("second line",
              source_map.span_to_snippet(ByteSpan(ByteLoc(24), ByteLoc(35))));
    EXPECT_EQ("third line",
              source_map.span_to_snippet(ByteSpan(ByteLoc(36), ByteLoc(46))));

    EXPECT_THROW(source_map.span_to_snippet(ByteSpan(ByteLoc(0), ByteLoc(46))),
                 cci::broken_contract);
    EXPECT_THROW(source_map.span_to_snippet(ByteSpan(ByteLoc(1), ByteLoc(0))),
                 cci::broken_contract);
}

TEST_F(SourceMapTest, byteLocToCharPos)
{
    const auto [fm1, chloc1] =
        source_map.byteloc_to_filemap_charloc(ByteLoc(0));
    EXPECT_EQ("block1.c", fm1.name);
    EXPECT_EQ(CharPos(0), chloc1);

    const auto [fm2, chloc2] =
        source_map.byteloc_to_filemap_charloc(ByteLoc(12));
    EXPECT_EQ("empty.c", fm2.name);
    EXPECT_EQ(CharPos(0), chloc2);

    const auto [fm3, chloc3] =
        source_map.byteloc_to_filemap_charloc(ByteLoc(15));
    EXPECT_EQ("block2.c", fm3.name);
    EXPECT_EQ(CharPos(2), chloc3);
}

TEST_F(SourceMapTest, ptrToByteLoc)
{
    const auto ptr1 = filemap1->src_view().data();
    EXPECT_EQ(ByteLoc(0), source_map.ptr_to_byteloc(filemap1->start_loc, ptr1));

    const auto ptr2 = filemap1->src_view().data() + 5;
    EXPECT_EQ(ByteLoc(5), source_map.ptr_to_byteloc(filemap1->start_loc, ptr2));

    const auto ptr3 = filemap2->src_view().data();
    EXPECT_EQ(ByteLoc(12),
              source_map.ptr_to_byteloc(filemap2->start_loc, ptr3));

    const auto ptr4 = filemap3->src_view().data();
    EXPECT_EQ(ByteLoc(13),
              source_map.ptr_to_byteloc(filemap3->start_loc, ptr4));

    // Misuse: trying to convert a pointer from a different file map.
    const auto ptr5 = filemap1->src_view().data();
    EXPECT_THROW(source_map.ptr_to_byteloc(filemap2->start_loc, ptr5),
                 cci::broken_contract);
}

TEST_F(SourceMapTest, multiByteCharByteLocToCharPos)
{
    const auto [fm1, chloc1] =
        mbc_source_map.byteloc_to_filemap_charloc(ByteLoc(0));
    EXPECT_EQ("mbc.c", fm1.name);
    EXPECT_EQ(CharPos(0), chloc1);

    const auto [fm2, chloc2] =
        mbc_source_map.byteloc_to_filemap_charloc(ByteLoc(2));
    EXPECT_EQ("mbc.c", fm2.name);
    EXPECT_EQ(CharPos(1), chloc2);

    const auto [fm3, chloc3] =
        mbc_source_map.byteloc_to_filemap_charloc(ByteLoc(26));
    EXPECT_EQ("mbc.c", fm3.name);
    EXPECT_EQ(CharPos(14), chloc3);

    const auto [fm4, chloc4] =
        mbc_source_map.byteloc_to_filemap_charloc(ByteLoc(29));
    EXPECT_EQ("mbc.c", fm4.name);
    EXPECT_EQ(CharPos(16), chloc4);

    EXPECT_THROW(mbc_source_map.byteloc_to_filemap_charloc(ByteLoc(1)),
                 cci::broken_contract);
    EXPECT_THROW(mbc_source_map.byteloc_to_filemap_charloc(ByteLoc(24)),
                 cci::broken_contract);
}

TEST_F(SourceMapTest, multiByteCharlookupSourceLocation)
{
    const auto loc1 = mbc_source_map.lookup_source_location(ByteLoc(0));
    EXPECT_EQ("mbc.c", loc1.file.name);
    EXPECT_EQ(1, loc1.line);
    EXPECT_EQ(CharPos(0), loc1.column);

    const auto loc2 = mbc_source_map.lookup_source_location(ByteLoc(2));
    EXPECT_EQ("mbc.c", loc2.file.name);
    EXPECT_EQ(1, loc2.line);
    EXPECT_EQ(CharPos(1), loc2.column);

    const auto loc3 = mbc_source_map.lookup_source_location(ByteLoc(31));
    EXPECT_EQ("mbc.c", loc3.file.name);
    EXPECT_EQ(2, loc3.line);
    EXPECT_EQ(CharPos(2), loc3.column);
}

} // namespace
