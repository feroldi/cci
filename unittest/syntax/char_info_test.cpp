#include "cci/syntax/char_info.hpp"
#include "gtest/gtest.h"

using namespace cci::charinfo;
using namespace cci;

TEST(CharInfoTest, isDigit)
{
  for (unsigned char c = '0'; c <= '9'; ++c)
    EXPECT_TRUE(is_digit(c));

  for (unsigned char c = 'a'; c <= 'z'; ++c)
    EXPECT_FALSE(is_digit(c));

  for (unsigned char c = 'A'; c <= 'Z'; ++c)
    EXPECT_FALSE(is_digit(c));

  for (unsigned char c = '!'; c <= '/'; ++c)
    EXPECT_FALSE(is_digit(c));

  for (unsigned char c = ':'; c <= '@'; ++c)
    EXPECT_FALSE(is_digit(c));

  for (unsigned char c = '['; c <= '`'; ++c)
    EXPECT_FALSE(is_digit(c));

  for (unsigned char c = '{'; c <= '~'; ++c)
    EXPECT_FALSE(is_digit(c));
}

TEST(CharInfoTest, isOctDigit)
{
  for (unsigned char c = '0'; c <= '7'; ++c)
    EXPECT_TRUE(is_octdigit(c));

  EXPECT_FALSE(is_octdigit('8'));
  EXPECT_FALSE(is_octdigit('9'));

  for (unsigned char c = 'a'; c <= 'z'; ++c)
    EXPECT_FALSE(is_octdigit(c));

  for (unsigned char c = 'A'; c <= 'Z'; ++c)
    EXPECT_FALSE(is_octdigit(c));

  for (unsigned char c = '!'; c <= '/'; ++c)
    EXPECT_FALSE(is_octdigit(c));

  for (unsigned char c = ':'; c <= '@'; ++c)
    EXPECT_FALSE(is_octdigit(c));

  for (unsigned char c = '['; c <= '`'; ++c)
    EXPECT_FALSE(is_octdigit(c));

  for (unsigned char c = '{'; c <= '~'; ++c)
    EXPECT_FALSE(is_octdigit(c));
}

TEST(CharInfoTest, isHexDigit)
{
  for (unsigned char c = '0'; c <= '9'; ++c)
    EXPECT_TRUE(is_hexdigit(c));

  for (unsigned char c = 'a'; c <= 'f'; ++c)
    EXPECT_TRUE(is_hexdigit(c));

  for (unsigned char c = 'g'; c <= 'z'; ++c)
    EXPECT_FALSE(is_hexdigit(c));

  for (unsigned char c = 'A'; c <= 'F'; ++c)
    EXPECT_TRUE(is_hexdigit(c));

  for (unsigned char c = 'G'; c <= 'Z'; ++c)
    EXPECT_FALSE(is_hexdigit(c));

  for (unsigned char c = '!'; c <= '/'; ++c)
    EXPECT_FALSE(is_hexdigit(c));

  for (unsigned char c = ':'; c <= '@'; ++c)
    EXPECT_FALSE(is_hexdigit(c));

  for (unsigned char c = '['; c <= '`'; ++c)
    EXPECT_FALSE(is_hexdigit(c));

  for (unsigned char c = '{'; c <= '~'; ++c)
    EXPECT_FALSE(is_hexdigit(c));
}

TEST(CharInfoTest, isAlpha)
{
  for (unsigned char c = '0'; c <= '9'; ++c)
    EXPECT_FALSE(is_alpha(c));

  for (unsigned char c = 'a'; c <= 'z'; ++c)
    EXPECT_TRUE(is_alpha(c));

  for (unsigned char c = 'A'; c <= 'Z'; ++c)
    EXPECT_TRUE(is_alpha(c));

  for (unsigned char c = '!'; c <= '/'; ++c)
    EXPECT_FALSE(is_alpha(c));

  for (unsigned char c = ':'; c <= '@'; ++c)
    EXPECT_FALSE(is_alpha(c));

  for (unsigned char c = '['; c <= '`'; ++c)
    EXPECT_FALSE(is_alpha(c));

  for (unsigned char c = '{'; c <= '~'; ++c)
    EXPECT_FALSE(is_alpha(c));
}

TEST(CharInfoTest, isAlphaNumeric)
{
  for (unsigned char c = '0'; c <= '9'; ++c)
    EXPECT_TRUE(is_alphanum(c));

  for (unsigned char c = 'a'; c <= 'z'; ++c)
    EXPECT_TRUE(is_alphanum(c));

  for (unsigned char c = 'A'; c <= 'Z'; ++c)
    EXPECT_TRUE(is_alphanum(c));

  for (unsigned char c = '!'; c <= '/'; ++c)
    EXPECT_FALSE(is_alphanum(c));

  for (unsigned char c = ':'; c <= '@'; ++c)
    EXPECT_FALSE(is_alphanum(c));

  for (unsigned char c = '['; c <= '`'; ++c)
    EXPECT_FALSE(is_alphanum(c));

  for (unsigned char c = '{'; c <= '~'; ++c)
    EXPECT_FALSE(is_alphanum(c));
}

TEST(CharInfoTest, isPrintable)
{
  for (unsigned char c = '0'; c <= '9'; ++c)
    EXPECT_TRUE(is_printable(c));

  for (unsigned char c = 'a'; c <= 'z'; ++c)
    EXPECT_TRUE(is_printable(c));

  for (unsigned char c = 'A'; c <= 'Z'; ++c)
    EXPECT_TRUE(is_printable(c));

  for (unsigned char c = '!'; c <= '/'; ++c)
    EXPECT_TRUE(is_printable(c));

  for (unsigned char c = ':'; c <= '@'; ++c)
    EXPECT_TRUE(is_printable(c));

  for (unsigned char c = '['; c <= '`'; ++c)
    EXPECT_TRUE(is_printable(c));

  for (unsigned char c = '{'; c <= '~'; ++c)
    EXPECT_TRUE(is_printable(c));


  EXPECT_FALSE(is_printable('\0'));
  EXPECT_FALSE(is_printable('\x7f'));
  EXPECT_FALSE(is_printable('\x80'));
  EXPECT_FALSE(is_printable('\xff'));
}

TEST(CharInfoTest, isASCII)
{
  EXPECT_TRUE(is_ascii('\0'));
  EXPECT_TRUE(is_ascii('a'));
  EXPECT_TRUE(is_ascii('0'));
  EXPECT_TRUE(is_ascii(' '));
  EXPECT_TRUE(is_ascii('.'));
  EXPECT_TRUE(is_ascii('_'));
  EXPECT_TRUE(is_ascii('\n'));
  EXPECT_TRUE(is_ascii('\x7f'));

  EXPECT_FALSE(is_ascii('\x80'));
  EXPECT_FALSE(is_ascii('\xff'));
}
