// Copyright Altua AS. All rights reserved.

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
