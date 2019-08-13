FROM gcc:9.2 AS builder

ARG CMAKE_VERSION=3.11.4
ENV CMAKE_BIN_PATH=$HOME/cmake

RUN mkdir $CMAKE_BIN_PATH \
    && wget --no-check-certificate -O - https://cmake.org/files/v${CMAKE_VERSION%.[0-9]}/cmake-$CMAKE_VERSION-Linux-x86_64.tar.gz \
    | tar --strip-components=1 -xz -C $CMAKE_BIN_PATH

ENV PATH="$CMAKE_BIN_PATH/bin:$PATH"

ARG GTEST_VERSION=1.8.0
ENV GTEST_ARCHIVE_PATH=$HOME/gtest.tar.gz
ENV GTEST_BIN_PATH=$HOME/gtest

RUN wget https://github.com/google/googletest/archive/release-$GTEST_VERSION.tar.gz -O $GTEST_ARCHIVE_PATH \
    && mkdir $GTEST_BIN_PATH \
    && tar xf $GTEST_ARCHIVE_PATH --strip-components=1 -C $GTEST_BIN_PATH \
    && (cd $GTEST_BIN_PATH/googletest && cmake -DBUILD_SHARED_LIBS=ON . && cmake --build .) \
    && cp -a $GTEST_BIN_PATH/googletest/include/gtest /usr/include \
    && cp -a $GTEST_BIN_PATH/googletest/libgtest_main.so /usr/lib/ \
    && cp -a $GTEST_BIN_PATH/googletest/libgtest.so /usr/lib/

COPY . /usr/src/cci
WORKDIR /usr/src/cci

RUN cmake .
RUN cmake --build .
RUN ctest --output-on-failure
