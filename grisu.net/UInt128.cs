// Copyright 2010 the V8 project authors. All rights reserved.
// Copyright 2011-2012, Kevin Ring. All rights reserved.
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


// This implementation is ported from the c++ implementation found at
// https://github.com/google/double-conversion/
// 

using System.Diagnostics;


namespace GrisuDotNet
{
    /// <summary>
    /// Represents a 128bit type. This class should be replaced by a native type on
    /// platforms that support 128bit integers.
    /// </summary>
    internal struct UInt128
    {
        private const ulong _kMask32 = 0xFFFFFFFF;

        // Value == (_highBits << 64) + _lowBits
        private ulong _highBits;
        private ulong _lowBits;


        internal UInt128(ulong high, ulong low)
        {
            _highBits = high;
            _lowBits = low;
        }


        internal int BitAt(int position)
        {
            if (position >= 64)
            {
                return (int) (_highBits >> (position - 64)) & 1;
            }
            else
            {
                return (int) (_lowBits >> position) & 1;
            }
        }


        /// <summary>
        /// Modifies *this to *this MOD (2^power).
        /// Returns *this DIV (2^power).
        /// </summary>
        internal int DivModPowerOf2(int power)
        {
            if (power >= 64)
            {
                int result = (int) (_highBits >> (power - 64));
                _highBits -= (ulong) (result) << (power - 64);
                return result;
            }
            else
            {
                ulong part_low = _lowBits >> power;
                ulong part_high = _highBits << (64 - power);
                int result = (int) (part_low + part_high);
                _highBits = 0;
                _lowBits -= part_low << power;
                return result;
            }
        }


        internal bool IsZero()
        {
            return _highBits == 0 && _lowBits == 0;
        }


        internal void Multiply(uint multiplicand)
        {
            ulong accumulator;

            accumulator = (_lowBits & _kMask32)*multiplicand;
            uint part = (uint) (accumulator & _kMask32);
            accumulator >>= 32;
            accumulator = accumulator + (_lowBits >> 32)*multiplicand;
            _lowBits = (accumulator << 32) + part;
            accumulator >>= 32;
            accumulator = accumulator + (_highBits & _kMask32)*multiplicand;
            part = (uint) (accumulator & _kMask32);
            accumulator >>= 32;
            accumulator = accumulator + (_highBits >> 32)*multiplicand;
            _highBits = (accumulator << 32) + part;
            Debug.Assert((accumulator >> 32) == 0);
        }


        internal void Shift(int shift_amount)
        {
            Debug.Assert(-64 <= shift_amount && shift_amount <= 64);
            if (shift_amount == 0)
            {
                return;
            }
            else if (shift_amount == -64)
            {
                _highBits = _lowBits;
                _lowBits = 0;
            }
            else if (shift_amount == 64)
            {
                _lowBits = _highBits;
                _highBits = 0;
            }
            else if (shift_amount <= 0)
            {
                _highBits <<= -shift_amount;
                _highBits += _lowBits >> (64 + shift_amount);
                _lowBits <<= -shift_amount;
            }
            else
            {
                _lowBits >>= shift_amount;
                _lowBits += _highBits << (64 - shift_amount);
                _highBits >>= shift_amount;
            }
        }
    }
}
