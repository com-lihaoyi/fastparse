#!/usr/bin/env bash
sbt ++2.10.6 \
    utilsJS/publishSigned \
    utilsJVM/publishSigned \
    fastparseJS/publishSigned \
    fastparseJVM/publishSigned \
    fastparseByteJS/publishSigned \
    fastparseByteJVM/publishSigned \
    scalaparseJS/publishSigned \
    scalaparseJVM/publishSigned \
    pythonparseJS/publishSigned \
    pythonparseJVM/publishSigned \
    cssparseJS/publishSigned \
    cssparseJVM/publishSigned \
    classparseJS/publishSigned \
    classparseJVM/publishSigned
sbt ++2.11.11 \
    utilsJS/publishSigned \
    utilsJVM/publishSigned \
    utilsNative/publishSigned \
    fastparseJS/publishSigned \
    fastparseJVM/publishSigned \
    fastparseNative/publishSigned \
    fastparseByteJS/publishSigned \
    fastparseByteJVM/publishSigned \
    fastparseByteNative/publishSigned \
    scalaparseJS/publishSigned \
    scalaparseJVM/publishSigned \
    scalaparseNative/publishSigned \
    pythonparseJS/publishSigned \
    pythonparseJVM/publishSigned \
    pythonparseNative/publishSigned \
    cssparseJS/publishSigned \
    cssparseJVM/publishSigned \
    cssparseNative/publishSigned \
    classparseJS/publishSigned \
    classparseJVM/publishSigned
    # Doesn't work!
    # classparseNative/publishSigned
sbt ++2.12.3 \
    utilsJS/publishSigned \
    utilsJVM/publishSigned \
    fastparseJS/publishSigned \
    fastparseJVM/publishSigned \
    fastparseByteJS/publishSigned \
    fastparseByteJVM/publishSigned \
    scalaparseJS/publishSigned \
    scalaparseJVM/publishSigned \
    pythonparseJS/publishSigned \
    pythonparseJVM/publishSigned \
    cssparseJS/publishSigned \
    cssparseJVM/publishSigned \
    classparseJS/publishSigned \
    classparseJVM/publishSigned
