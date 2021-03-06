﻿// Copyright 2006-2008 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// This file contains 100.000 decimal representations of random doubles. They
// have been generated using Gay's dtoa to produce the shortest representation:
//          decimal_rep = dtoa(v, 0, 0, &decimal_point, &sign, NULL);

using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Altua.UnitTesting;
using GrisuDotNet;
using Xunit;
using Xunit.Sdk;

namespace Tests
{
    public sealed class GayShortest
    {
        [Fact]
        internal void Grisu_Shortest_DoubleToAscii_HasCorrectDecimalPoint()
        {
            int length, point;
            bool sign;
            char[] buffer = new char[Grisu.kBase10MaximalLength + 1];


            foreach (var input in GayShortestData.KShortestTestNumbers().Take(1))
            {
                Grisu.DoubleToAscii(input.Value, DtoaMode.SHORTEST, -1, ref buffer, out sign, out length, out point);

                Assert.Equal(input.DecimalPoint, point);
            }
        }


        [Fact]
        internal void Grisu_Shortest_DoubleToAscii_HasCorrectRepresentation()
        {
            int length, point;
            bool sign;
            char[] buffer = new char[Grisu.kBase10MaximalLength + 1];

            foreach (var input in GayShortestData.KShortestTestNumbers())
            {
                Grisu.DoubleToAscii(input.Value, DtoaMode.SHORTEST, -1, ref buffer, out sign, out length, out point);

                char[] expected = input.Representation.ToCharArray();
                char[] actual = new char[length];
                Array.Copy(buffer, 0, actual, 0, length);
                AssertSequence.Equal(expected, actual);
            }
        }

    }
}
