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

using System;
using System.Diagnostics;
using System.Globalization;
using System.IO;

namespace GrisuDotNet
{
    /// <summary>
    /// The supported modes for double to ascii conversion.
    /// </summary>
    public enum DtoaMode
    {
        /// <summary>
        /// Produce the shortest correct representation.
        /// For example the output of 0.299999999999999988897 is (the less accurate
        /// but correct) 0.3.
        /// </summary>
        SHORTEST,
        /// <summary>
        /// Same as SHORTEST, but for single-precision floats.
        /// </summary>
        SHORTEST_SINGLE,
        /// <summary>
        /// Produce a fixed number of digits after the decimal point.
        /// For instance fixed(0.1, 4) becomes 0.1000
        /// If the input number is big, the output will be big.
        /// </summary>
        FIXED,
        /// <summary>
        /// Fixed number of digits (independent of the decimal point).
        /// </summary>
        PRECISION
    };

    public enum FastDtoaMode
    {
        /// <summary>
        /// Computes the shortest representation of the given input. The returned
        /// result will be the most accurate number of this length. Longer
        /// representations might be more accurate.
        /// </summary>
        SHORTEST,
        /// <summary>
        /// Same as FAST_DTOA_SHORTEST but for single-precision floats.
        /// </summary>
        SHORTEST_SINGLE,
        /// <summary>
        /// Computes a representation where the precision (number of digits) is
        /// given as input. The precision is independent of the decimal point.
        /// </summary>
        PRECISION
    };

public static class Grisu
    {
        [ThreadStatic]
        private static char[] ts_decimal_rep;

        private const int kDoubleSignificandSize = 53;  // Includes the hidden bit.

        public static void DoubleToAsciiShortest(double value, out char[] buffer, out bool sign, out int length, out int point)
        {
            buffer = new char[kBase10MaximalLength + 1];
            DoubleToAscii(value, DtoaMode.SHORTEST, 0, ref buffer, out sign, out length, out point);
        }

        /// <summary>
        /// Converts the given double 'v' to ascii. 'v' must not be NaN, +Infinity, or
        /// -Infinity. 
        ///
        ///  The result should be interpreted as buffer * 10^(point-length).
        ///
        /// The output depends on the given mode:
        ///  - SHORTEST: produce the least amount of digits for which the internal
        ///   identity requirement is still satisfied. If the digits are printed
        ///   (together with the correct exponent) then reading this number will give
        ///   'v' again. The buffer will choose the representation that is closest to
        ///   'v'. If there are two at the same distance, than the one farther away
        ///   from 0 is chosen (halfway cases - ending with 5 - are rounded up).
        ///   In this mode the 'requested_digits' parameter is ignored.
        ///  - SHORTEST_SINGLE: same as SHORTEST but with single-precision.
        ///  - FIXED: produces digits necessary to print a given number with
        ///   'requested_digits' digits after the decimal point. The produced digits
        ///   might be too short in which case the caller has to fill the remainder
        ///   with '0's.
        ///   Example: toFixed(0.001, 5) is allowed to return buffer="1", point=-2.
        ///   Halfway cases are rounded towards +/-Infinity (away from 0). The call
        ///   toFixed(0.15, 2) thus returns buffer="2", point=0.
        ///   The returned buffer may contain digits that would be truncated from the
        ///   shortest representation of the input.
        ///  - PRECISION: produces 'requested_digits' where the first digit is not '0'.
        ///   Even though the length of produced digits usually equals
        ///   'requested_digits', the function is allowed to return fewer digits, in
        ///   which case the caller has to fill the missing digits with '0's.
        ///   Halfway cases are again rounded away from 0.
        /// DoubleToAscii expects the given buffer to be big enough to hold all
        /// digits and a terminating null-character. In SHORTEST-mode it expects a
        /// buffer of at least kBase10MaximalLength + 1. In all other modes the
        /// requested_digits parameter and the padding-zeroes limit the size of the
        /// output. Don't forget the decimal point, the exponent character and the
        /// terminating null-character when computing the maximal output size.
        /// The given length is only used in debug mode to ensure the buffer is big
        /// enough.
        /// </summary>
        public static void DoubleToAscii(
                                        double value, 
                                        DtoaMode mode,
                                        int requested_digits,
                                        ref char[] buffer,
                                        out bool sign,
                                        out int length,
                                        out int point)
        {
            Debug.Assert(!(double.IsNaN(value) || double.IsInfinity(value)));
            Debug.Assert(mode == DtoaMode.SHORTEST || mode == DtoaMode.SHORTEST_SINGLE || requested_digits >= 0);

            if (value < 0)
            {
                sign = true;
                value = -value;
            }
            else
            {
                sign = false;
            }

            if (mode == DtoaMode.PRECISION && requested_digits == 0)
            {
                buffer[0] = '\0';
                length = 0;
                point = 0;
                return;
            }

            // ReSharper disable once CompareOfFloatsByEqualityOperator
            if (value == 0)
            {
                buffer[0] = '0';
                buffer[1] = '\0';
                length = 1;
                point = 1;
                return;
            }

            bool fastWorked;
            GrisuDouble gv = new GrisuDouble(value);
            switch (mode)
            {
                case DtoaMode.SHORTEST:
                    fastWorked = FastDtoa(gv, FastDtoaMode.SHORTEST, 0, buffer, out length, out point);
                    break;
                case DtoaMode.SHORTEST_SINGLE:
                    fastWorked = FastDtoa(gv, FastDtoaMode.SHORTEST_SINGLE, 0,
                                           buffer, out length, out point);
                    break;
                case DtoaMode.FIXED:
                    fastWorked = FastFixedDtoa(gv, requested_digits, buffer, out length, out point);
                    break;
                case DtoaMode.PRECISION:
                    fastWorked = FastDtoa(gv, FastDtoaMode.PRECISION, requested_digits, buffer, out length, out point);
                    break;
                default:
                    throw new InvalidOperationException($"The mode '{mode}' is not supported in {nameof(DoubleToAscii)}.");
            }
            if (fastWorked) return;

            // If the fast dtoa didn't succeed use the slower bignum version.
            // TODO UNDONE
            //BignumDtoaMode bignum_mode = DtoaToBignumDtoaMode(mode);
            //BignumDtoa(value, bignum_mode, requested_digits, buffer, length, point);
            buffer[length] = '\0';
        }

        private static bool FastFixedDtoa(GrisuDouble v,
                   int fractional_count,
                   char[] buffer,
                   out int length,
                   out int decimal_point)
        {
            const uint kMaxUInt32 = 0xFFFFFFFF;
            ulong significand = v.Significand;
            int exponent = v.Exponent;
            // v = significand * 2^exponent (with significand a 53bit integer).
            // If the exponent is larger than 20 (i.e. we may have a 73bit number) then we
            // don't know how to compute the representation. 2^73 ~= 9.5*10^21.
            // If necessary this limit could probably be increased, but we don't need
            // more.
            length = 0;
            decimal_point = 0;
            if (exponent > 20) return false;
            if (fractional_count > 20) return false;
            // At most kDoubleSignificandSize bits of the significand are non-zero.
            // Given a 64 bit integer we have 11 0s followed by 53 potentially non-zero
            // bits:  0..11*..0xxx..53*..xx
            if (exponent + kDoubleSignificandSize > 64)
            {
                // The exponent must be > 11.
                //
                // We know that v = significand * 2^exponent.
                // And the exponent > 11.
                // We simplify the task by dividing v by 10^17.
                // The quotient delivers the first digits, and the remainder fits into a 64
                // bit number.
                // Dividing by 10^17 is equivalent to dividing by 5^17*2^17.
                const ulong kFive17 = 0xB1A2BC2EC5;  // 5^17
                ulong divisor = kFive17;
                int divisor_power = 17;
                ulong dividend = significand;
                uint quotient;
                ulong remainder;
                // Let v = f * 2^e with f == significand and e == exponent.
                // Then need q (quotient) and r (remainder) as follows:
                //   v            = q * 10^17       + r
                //   f * 2^e      = q * 10^17       + r
                //   f * 2^e      = q * 5^17 * 2^17 + r
                // If e > 17 then
                //   f * 2^(e-17) = q * 5^17        + r/2^17
                // else
                //   f  = q * 5^17 * 2^(17-e) + r/2^e
                if (exponent > divisor_power)
                {
                    // We only allow exponents of up to 20 and therefore (17 - e) <= 3
                    dividend <<= exponent - divisor_power;
                    quotient = (uint)(dividend / divisor);
                    remainder = (dividend % divisor) << divisor_power;
                }
                else
                {
                    divisor <<= divisor_power - exponent;
                    quotient = (uint)(dividend / divisor);
                    remainder = (dividend % divisor) << exponent;
                }
                FillDigits32(quotient, buffer, ref length);
                FillDigits64FixedLength(remainder, buffer, ref length);
                decimal_point = length;
            }
            else if (exponent >= 0)
            {
                // 0 <= exponent <= 11
                significand <<= exponent;
                FillDigits64(significand, buffer, ref length);
                decimal_point = length;
            }
            else if (exponent > -kDoubleSignificandSize)
            {
                // We have to cut the number.
                ulong integrals = significand >> -exponent;
                ulong fractionals = significand - (integrals << -exponent);
                if (integrals > kMaxUInt32)
                {
                    FillDigits64(integrals, buffer, ref length);
                }
                else
                {
                    FillDigits32((uint)(integrals), buffer, ref length);
                }
                decimal_point = length;
                FillFractionals(fractionals, exponent, fractional_count,
                                buffer, ref length, ref decimal_point);
            }
            else if (exponent < -128)
            {
                // This configuration (with at most 20 digits) means that all digits must be
                // 0.
                Debug.Assert(fractional_count <= 20);
                buffer[0] = '\0';
                length = 0;
                decimal_point = -fractional_count;
            }
            else
            {
                decimal_point = 0;
                FillFractionals(significand, exponent, fractional_count,
                                buffer, ref length, ref decimal_point);
            }
            TrimZeros(buffer, ref length, ref decimal_point);
            buffer[length] = '\0';
            if (length == 0)
            {
                // The string is empty and the decimal_point thus has no importance. Mimick
                // Gay's dtoa and and set it to -fractional_count.
                decimal_point = -fractional_count;
            }
            return true;
        }

        /// <summary>
        /// Removes leading and trailing zeros.
        /// If leading zeros are removed then the decimal point position is adjusted.
        /// </summary>
        private static void TrimZeros(char[] buffer, ref int length, ref int decimal_point)
        {
            while (length > 0 && buffer[length - 1] == '0')
            {
                length--;
            }
            int first_non_zero = 0;
            while (first_non_zero < length && buffer[first_non_zero] == '0')
            {
                first_non_zero++;
            }
            if (first_non_zero != 0)
            {
                for (int i = first_non_zero; i < length; ++i)
                {
                    buffer[i - first_non_zero] = buffer[i];
                }
                length -= first_non_zero;
                decimal_point -= first_non_zero;
            }
        }


        // The given fractionals number represents a fixed-point number with binary
        // point at bit (-exponent).
        // Preconditions:
        //   -128 <= exponent <= 0.
        //   0 <= fractionals * 2^exponent < 1
        //   The buffer holds the result.
        // The function will round its result. During the rounding-process digits not
        // generated by this function might be updated, and the decimal-point variable
        // might be updated. If this function generates the digits 99 and the buffer
        // already contained "199" (thus yielding a buffer of "19999") then a
        // rounding-up will change the contents of the buffer to "20000".
        private static void FillFractionals(ulong fractionals, int exponent,
                                    int fractional_count, char[] buffer,
                                    ref int length,  ref int decimal_point)
        {
            Debug.Assert(-128 <= exponent && exponent <= 0);
            // 'fractionals' is a fixed-point number, with binary point at bit
            // (-exponent). Inside the function the non-converted remainder of fractionals
            // is a fixed-point number, with binary point at bit 'point'.
            if (-exponent <= 64)
            {
                // One 64 bit number is sufficient.
                Debug.Assert(fractionals >> 56 == 0);
                int point = -exponent;
                for (int i = 0; i < fractional_count; ++i)
                {
                    if (fractionals == 0) break;
                    // Instead of multiplying by 10 we multiply by 5 and adjust the point
                    // location. This way the fractionals variable will not overflow.
                    // Invariant at the beginning of the loop: fractionals < 2^point.
                    // Initially we have: point <= 64 and fractionals < 2^56
                    // After each iteration the point is decremented by one.
                    // Note that 5^3 = 125 < 128 = 2^7.
                    // Therefore three iterations of this loop will not overflow fractionals
                    // (even without the subtraction at the end of the loop body). At this
                    // time point will satisfy point <= 61 and therefore fractionals < 2^point
                    // and any further multiplication of fractionals by 5 will not overflow.
                    fractionals *= 5;
                    point--;
                    int digit = (int)(fractionals >> point);
                    Debug.Assert(digit <= 9);
                    buffer[length] =(char)('0' + digit);
                    length++;
                    fractionals -= (ulong)(digit) << point;
                }
                // If the first bit after the point is set we have to round up.
                if (((fractionals >> (point - 1)) & 1) == 1)
                {
                    RoundUp(buffer, ref length, ref decimal_point);
                }
            }
            else
            {  // We need 128 bits.
                Debug.Assert(64 < -exponent && -exponent <= 128);
                UInt128 fractionals128 = new UInt128(fractionals, 0);
                fractionals128.Shift(-exponent - 64);
                int point = 128;
                for (int i = 0; i < fractional_count; ++i)
                {
                    if (fractionals128.IsZero()) break;
                    // As before: instead of multiplying by 10 we multiply by 5 and adjust the
                    // point location.
                    // This multiplication will not overflow for the same reasons as before.
                    fractionals128.Multiply(5);
                    point--;
                    int digit = fractionals128.DivModPowerOf2(point);
                    Debug.Assert(digit <= 9);
                    buffer[length] = (char)('0' + digit);
                    length++;
                }
                if (fractionals128.BitAt(point - 1) == 1)
                {
                    RoundUp(buffer, ref length, ref decimal_point);
                }
            }
        }

        private static void RoundUp(char[] buffer, ref int length, ref int decimal_point)
        {
            // An empty buffer represents 0.
            if (length == 0)
            {
                buffer[0] = '1';
                decimal_point = 1;
                length = 1;
                return;
            }
            // Round the last digit until we either have a digit that was not '9' or until
            // we reached the first digit.
            buffer[length - 1]++;
            for (int i = length - 1; i > 0; --i)
            {
                if (buffer[i] != '0' + 10)
                {
                    return;
                }
                buffer[i] = '0';
                buffer[i - 1]++;
            }
            // If the first digit is now '0' + 10, we would need to set it to '0' and add
            // a '1' in front. However we reach the first digit only if all following
            // digits had been '9' before rounding up. Now all trailing digits are '0' and
            // we simply switch the first digit to '1' and update the decimal-point
            // (indicating that the point is now one digit to the right).
            if (buffer[0] == '0' + 10)
            {
                buffer[0] = '1';
                decimal_point++;
            }
        }

        private static void FillDigits32(uint number, char[] buffer, ref int length)
        {
            int number_length = 0;
            // We fill the digits in reverse order and exchange them afterwards.
            while (number != 0)
            {
                uint digit = number % 10;
                number /= 10;
                buffer[length + number_length] = (char)('0' + digit);
                number_length++;
            }
            // Exchange the digits.
            int i = length;
            int j = length + number_length - 1;
            while (i < j)
            {
                char tmp = buffer[i];
                buffer[i] = buffer[j];
                buffer[j] = tmp;
                i++;
                j--;
            }
            length += number_length;
        }

        static void FillDigits64(ulong number, char[] buffer, ref int length)
        {
            const uint kTen7 = 10000000;
            // For efficiency cut the number into 3 uint32_t parts, and print those.
            uint part2 = (uint)(number % kTen7);
            number /= kTen7;
            uint part1 = (uint)(number % kTen7);
            uint part0 = (uint)(number / kTen7);

            if (part0 != 0)
            {
                FillDigits32(part0, buffer, ref length);
                FillDigits32FixedLength(part1, 7, buffer, ref length);
                FillDigits32FixedLength(part2, 7, buffer, ref length);
            }
            else if (part1 != 0)
            {
                FillDigits32(part1, buffer, ref length);
                FillDigits32FixedLength(part2, 7, buffer, ref length);
            }
            else
            {
                FillDigits32(part2, buffer, ref length);
            }
        }

        static void FillDigits64FixedLength(ulong number, char[] buffer, ref int length)
        {
            const uint kTen7 = 10000000;
            // For efficiency cut the number into 3 uint32_t parts, and print those.
            uint part2 = (uint)(number % kTen7);
            number /= kTen7;
            uint part1 = (uint)(number % kTen7);
            uint part0 = (uint)(number / kTen7);

            FillDigits32FixedLength(part0, 3, buffer, ref length);
            FillDigits32FixedLength(part1, 7, buffer, ref length);
            FillDigits32FixedLength(part2, 7, buffer, ref length);
        }

        static void FillDigits32FixedLength(uint number, int requested_length,
                                    char[] buffer, ref int length)
        {
            for (int i = requested_length - 1; i >= 0; --i)
            {
                buffer[length + i] = (char)('0' + number % 10);
                number /= 10;
            }
            length += requested_length;
        }



        private static bool FastDtoa(GrisuDouble v, FastDtoaMode mode, int requested_digits, char[] buffer,
              out int length,
              out int decimal_point)
        {
            Debug.Assert(v.Value > 0);
            Debug.Assert(!v.IsSpecial);

            bool result = false;
            int decimal_exponent = 0;
            decimal_point = 0;
            switch (mode)
            {
                case FastDtoaMode.SHORTEST:
                case FastDtoaMode.SHORTEST_SINGLE:
                    result = Grisu3(v, mode, buffer, out length, out decimal_exponent);
                    break;
                case FastDtoaMode.PRECISION:
                    result = Grisu3Counted(v, requested_digits, buffer, out length, out decimal_exponent);
                    break;
                default:
                    throw new InvalidOperationException($"The mode '{mode}' is not supported in {nameof(FastDtoa)}.");
            }
            if (result)
            {
                decimal_point = length + decimal_exponent;
                buffer[length] = '\0';
            }
            return result;
        }


        public static void DoubleToString(double value, TextWriter writer)
        {
            if (value < 0.0)
            {
                writer.Write('-');
                value = -value;
            }

            GrisuDouble grisuDouble = new GrisuDouble(value);
            if (grisuDouble.IsSpecial)
            {
                HandleSpecialValues(ref grisuDouble, writer);
                return;
            }

            char[] decimal_rep = ts_decimal_rep;
            if (decimal_rep == null)
                decimal_rep = ts_decimal_rep = new char[kBase10MaximalLength + 1];

            int decimal_point;
            int decimal_rep_length;

            if (!DoubleToShortestAscii(grisuDouble, decimal_rep, out decimal_rep_length, out decimal_point))
            {
                writer.Write(string.Format(CultureInfo.InvariantCulture, "{0:R}", value));
                return;
            }

            int decimalRepLength = decimal_rep_length;
            if (decimal_point < 1)
            {
                decimalRepLength += -decimal_point + 1;
            }
            else if (decimal_point >= decimal_rep_length)
            {
                decimalRepLength += decimal_point - decimal_rep_length + 1;
            }

            int exponent = decimal_point - 1;
            int absExponent = Math.Abs(exponent);
            int exponentRepLength = decimal_rep_length + 3;
            if (exponent < 0)
                ++exponentRepLength;
            if (absExponent >= 10)
            {
                ++exponentRepLength;
                if (absExponent >= 100)
                    ++exponentRepLength;
            }

            if (decimalRepLength <= exponentRepLength)
            {
                CreateDecimalRepresentation(decimal_rep, decimal_rep_length,
                                            decimal_point,
                                            Math.Max(0, decimal_rep_length - decimal_point),
                                            writer);
            }
            else
            {
                CreateExponentialRepresentation(decimal_rep, decimal_rep_length, exponent,
                                                writer);
            }
        }

        // The maximal number of digits that are needed to emit a double in base 10.
        // A higher precision can be achieved by using more digits, but the shortest
        // accurate representation of any double will never use more digits than
        // kBase10MaximalLength.
        // Note that DoubleToAscii null-terminates its input. So the given buffer
        // should be at least kBase10MaximalLength + 1 characters long.
        internal const int kBase10MaximalLength = 17;

        private const string infinity_symbol_ = "Infinity";
        private const string nan_symbol_ = "NaN";
        private const char exponent_character_ = 'e';

        private static void HandleSpecialValues(
            ref GrisuDouble double_inspect,
            TextWriter writer)
        {
            if (double_inspect.IsInfinite)
            {
                if (double_inspect.Value < 0)
                {
                    writer.Write('-');
                }
                writer.Write(infinity_symbol_);
                return;
            }
            if (double_inspect.IsNaN)
            {
                writer.Write(nan_symbol_);
                return;
            }
        }

        private static bool DoubleToShortestAscii(GrisuDouble v, char[] buffer, out int length, out int point)
        {
            Debug.Assert(!v.IsSpecial);
            Debug.Assert(v.Value >= 0.0);

            double value = v.Value;

            if (value == 0.0)
            {
                buffer[0] = '0';
                buffer[1] = '\0';
                length = 1;
                point = 1;
                return true;
            }

            int decimal_exponent;
            bool result = Grisu3(v, FastDtoaMode.SHORTEST, buffer, out length, out decimal_exponent);
            if (result)
            {
                point = length + decimal_exponent;
            }
            else
            {
                point = 0;
            }
            return result;
        }

        // The minimal and maximal target exponent define the range of w's binary
        // exponent, where 'w' is the result of multiplying the input by a cached power
        // of ten.
        //
        // A different range might be chosen on a different platform, to optimize digit
        // generation, but a smaller range requires more powers of ten to be cached.
        private const int kMinimalTargetExponent = -60;
        private const int kMaximalTargetExponent = -32;

        // Provides a decimal representation of v.
        // Returns true if it succeeds, otherwise the result cannot be trusted.
        // There will be *length digits inside the buffer (not null-terminated).
        // If the function returns true then
        //        v == (double) (buffer * 10^decimal_exponent).
        // The digits in the buffer are the shortest representation possible: no
        // 0.09999999999999999 instead of 0.1. The shorter representation will even be
        // chosen even if the longer one would be closer to v.
        // The last digit will be closest to the actual v. That is, even if several
        // digits might correctly yield 'v' when read again, the closest will be
        // computed.
        private static bool Grisu3(GrisuDouble v,
                           FastDtoaMode mode,
                           char[] buffer,
                           out int length,
                           out int decimal_exponent)
        {
            DiyFp w = v.AsNormalizedDiyFp();
            // boundary_minus and boundary_plus are the boundaries between v and its
            // closest floating-point neighbors. Any number strictly between
            // boundary_minus and boundary_plus will round to v when convert to a double.
            // Grisu3 will never output representations that lie exactly on a boundary.
            DiyFp boundary_minus, boundary_plus;
            if (mode == FastDtoaMode.SHORTEST)
            {
                v.NormalizedBoundaries(out boundary_minus, out boundary_plus);
            }
            else
            {
                Debug.Assert(mode == FastDtoaMode.SHORTEST_SINGLE);
                throw new NotImplementedException($"The mode '{FastDtoaMode.SHORTEST_SINGLE}' is currently not implemented.");
                //float single_v = static_cast<float>(v);
                //Single(single_v).NormalizedBoundaries(&boundary_minus, &boundary_plus);
            }
            Debug.Assert(boundary_plus.E == w.E);
            DiyFp ten_mk;  // Cached power of ten: 10^-k
            int mk;        // -k
            int ten_mk_minimal_binary_exponent =
               kMinimalTargetExponent - (w.E + DiyFp.kSignificandSize);
            int ten_mk_maximal_binary_exponent =
               kMaximalTargetExponent - (w.E + DiyFp.kSignificandSize);
            PowersOfTenCache.GetCachedPowerForBinaryExponentRange(
                ten_mk_minimal_binary_exponent,
                ten_mk_maximal_binary_exponent,
                out ten_mk, out mk);
            Debug.Assert((kMinimalTargetExponent <= w.E + ten_mk.E +
                    DiyFp.kSignificandSize) &&
                   (kMaximalTargetExponent >= w.E + ten_mk.E +
                    DiyFp.kSignificandSize));
            // Note that ten_mk is only an approximation of 10^-k. A DiyFp only contains a
            // 64 bit significand and ten_mk is thus only precise up to 64 bits.

            // The DiyFp.Times procedure rounds its result, and ten_mk is approximated
            // too. The variable scaled_w (as well as scaled_boundary_minus/plus) are now
            // off by a small amount.
            // In fact: scaled_w - w*10^k < 1ulp (unit in the last place) of scaled_w.
            // In other words: let f = scaled_w.f() and e = scaled_w.e(), then
            //           (f-1) * 2^e < w*10^k < (f+1) * 2^e
            //DiyFp scaled_w = DiyFp.Times(ref w, ref ten_mk);
            w.Multiply(ref ten_mk);
            Debug.Assert(w.E ==
                   boundary_plus.E + ten_mk.E + DiyFp.kSignificandSize);
            // In theory it would be possible to avoid some recomputations by computing
            // the difference between w and boundary_minus/plus (a power of 2) and to
            // compute scaled_boundary_minus/plus by subtracting/adding from
            // scaled_w. However the code becomes much less readable and the speed
            // enhancements are not terriffic.
            //DiyFp scaled_boundary_minus = DiyFp.Times(ref boundary_minus, ref ten_mk);
            boundary_minus.Multiply(ref ten_mk);
            //DiyFp scaled_boundary_plus = DiyFp.Times(ref boundary_plus, ref ten_mk);
            boundary_plus.Multiply(ref ten_mk);

            // DigitGen will generate the digits of scaled_w. Therefore we have
            // v == (double) (scaled_w * 10^-mk).
            // Set decimal_exponent == -mk and pass it to DigitGen. If scaled_w is not an
            // integer than it will be updated. For instance if scaled_w == 1.23 then
            // the buffer will be filled with "123" und the decimal_exponent will be
            // decreased by 2.
            int kappa;
            bool result = DigitGen(ref boundary_minus, ref w, ref boundary_plus,
                                   buffer, out length, out kappa);
            decimal_exponent = -mk + kappa;
            return result;
        }

        // The "counted" version of grisu3 (see above) only generates requested_digits
        // number of digits. This version does not generate the shortest representation,
        // and with enough requested digits 0.1 will at some point print as 0.9999999...
        // Grisu3 is too imprecise for real halfway cases (1.5 will not work) and
        // therefore the rounding strategy for halfway cases is irrelevant.
        static bool Grisu3Counted(GrisuDouble v,
                                  int requested_digits,
                                  char[] buffer,
                                  out int length,
                                  out int decimal_exponent)
        {
            DiyFp w = v.AsNormalizedDiyFp();
            DiyFp ten_mk;  // Cached power of ten: 10^-k
            int mk;        // -k
            int ten_mk_minimal_binary_exponent =
               kMinimalTargetExponent - (w.E + DiyFp.kSignificandSize);
            int ten_mk_maximal_binary_exponent =
               kMaximalTargetExponent - (w.E + DiyFp.kSignificandSize);
            PowersOfTenCache.GetCachedPowerForBinaryExponentRange(
                ten_mk_minimal_binary_exponent,
                ten_mk_maximal_binary_exponent,
                out ten_mk, out mk);
            Debug.Assert((kMinimalTargetExponent <= w.E + ten_mk.E +
                    DiyFp.kSignificandSize) &&
                   (kMaximalTargetExponent >= w.E + ten_mk.E +
                    DiyFp.kSignificandSize));
            // Note that ten_mk is only an approximation of 10^-k. A DiyFp only contains a
            // 64 bit significand and ten_mk is thus only precise up to 64 bits.

            // The DiyFp::Times procedure rounds its result, and ten_mk is approximated
            // too. The variable scaled_w (as well as scaled_boundary_minus/plus) are now
            // off by a small amount.
            // In fact: scaled_w - w*10^k < 1ulp (unit in the last place) of scaled_w.
            // In other words: let f = scaled_w.f() and e = scaled_w.e(), then
            //           (f-1) * 2^e < w*10^k < (f+1) * 2^e
            w.Multiply(ref ten_mk);

            // We now have (double) (scaled_w * 10^-mk).
            // DigitGen will generate the first requested_digits digits of scaled_w and
            // return together with a kappa such that scaled_w ~= buffer * 10^kappa. (It
            // will not always be exactly the same since DigitGenCounted only produces a
            // limited number of digits.)
            int kappa;
            bool result = DigitGenCounted(w, requested_digits,
                                          buffer, out length, out kappa);
            decimal_exponent = -mk + kappa;
            return result;
        }

        // Generates the digits of input number w.
        // w is a floating-point number (DiyFp), consisting of a significand and an
        // exponent. Its exponent is bounded by kMinimalTargetExponent and
        // kMaximalTargetExponent.
        //       Hence -60 <= w.e() <= -32.
        //
        // Returns false if it fails, in which case the generated digits in the buffer
        // should not be used.
        // Preconditions:
        //  * low, w and high are correct up to 1 ulp (unit in the last place). That
        //    is, their error must be less than a unit of their last digits.
        //  * low.e() == w.e() == high.e()
        //  * low < w < high, and taking into account their error: low~ <= high~
        //  * kMinimalTargetExponent <= w.e() <= kMaximalTargetExponent
        // Postconditions: returns false if procedure fails.
        //   otherwise:
        //     * buffer is not null-terminated, but len contains the number of digits.
        //     * buffer contains the shortest possible decimal digit-sequence
        //       such that LOW < buffer * 10^kappa < HIGH, where LOW and HIGH are the
        //       correct values of low and high (without their error).
        //     * if more than one decimal representation gives the minimal number of
        //       decimal digits then the one closest to W (where W is the correct value
        //       of w) is chosen.
        // Remark: this procedure takes into account the imprecision of its input
        //   numbers. If the precision is not enough to guarantee all the postconditions
        //   then false is returned. This usually happens rarely (~0.5%).
        //
        // Say, for the sake of example, that
        //   w.e() == -48, and w.f() == 0x1234567890abcdef
        // w's value can be computed by w.f() * 2^w.e()
        // We can obtain w's integral digits by simply shifting w.f() by -w.e().
        //  -> w's integral part is 0x1234
        //  w's fractional part is therefore 0x567890abcdef.
        // Printing w's integral part is easy (simply print 0x1234 in decimal).
        // In order to print its fraction we repeatedly multiply the fraction by 10 and
        // get each digit. Example the first digit after the point would be computed by
        //   (0x567890abcdef * 10) >> 48. -> 3
        // The whole thing becomes slightly more complicated because we want to stop
        // once we have enough digits. That is, once the digits inside the buffer
        // represent 'w' we can stop. Everything inside the interval low - high
        // represents w. However we have to pay attention to low, high and w's
        // imprecision.
        private static bool DigitGen(ref DiyFp low,
                             ref DiyFp w,
                             ref DiyFp high,
                             char[] buffer,
                             out int length,
                             out int kappa)
        {
            Debug.Assert(low.E == w.E && w.E == high.E);
            Debug.Assert(low.F + 1 <= high.F - 1);
            Debug.Assert(kMinimalTargetExponent <= w.E && w.E <= kMaximalTargetExponent);
            // low, w and high are imprecise, but by less than one ulp (unit in the last
            // place).
            // If we remove (resp. add) 1 ulp from low (resp. high) we are certain that
            // the new numbers are outside of the interval we want the final
            // representation to lie in.
            // Inversely adding (resp. removing) 1 ulp from low (resp. high) would yield
            // numbers that are certain to lie in the interval. We will use this fact
            // later on.
            // We will now start by generating the digits within the uncertain
            // interval. Later we will weed out representations that lie outside the safe
            // interval and thus _might_ lie outside the correct interval.
            ulong unit = 1;
            DiyFp too_low = new DiyFp(low.F - unit, low.E);
            DiyFp too_high = new DiyFp(high.F + unit, high.E);
            // too_low and too_high are guaranteed to lie outside the interval we want the
            // generated number in.
            DiyFp unsafe_interval = DiyFp.Minus(ref too_high, ref too_low);
            // We now cut the input number into two parts: the integral digits and the
            // fractionals. We will not write any decimal separator though, but adapt
            // kappa instead.
            // Reminder: we are currently computing the digits (stored inside the buffer)
            // such that:   too_low < buffer * 10^kappa < too_high
            // We use too_high for the digit_generation and stop as soon as possible.
            // If we stop early we effectively round down.
            DiyFp one = new DiyFp(1UL << -w.E, w.E);
            // Division by one is a shift.
            uint integrals = (uint)(too_high.F >> -one.E);
            // Modulo by one is an and.
            ulong fractionals = too_high.F & (one.F - 1);
            uint divisor;
            int divisor_exponent_plus_one;
            BiggestPowerTen(integrals, DiyFp.kSignificandSize - (-one.E),
                            out divisor, out divisor_exponent_plus_one);
            kappa = divisor_exponent_plus_one;
            length = 0;
            // Loop invariant: buffer = too_high / 10^kappa  (integer division)
            // The invariant holds for the first iteration: kappa has been initialized
            // with the divisor exponent + 1. And the divisor is the biggest power of ten
            // that is smaller than integrals.
            ulong unsafeIntervalF = unsafe_interval.F;
            while (kappa > 0)
            {
                int digit = (int)(integrals / divisor);
                buffer[length] = (char)('0' + digit);
                ++length;
                integrals %= divisor;
                kappa--;
                // Note that kappa now equals the exponent of the divisor and that the
                // invariant thus holds again.
                ulong rest =
                    ((ulong)(integrals) << -one.E) + fractionals;
                // Invariant: too_high = buffer * 10^kappa + DiyFp(rest, one.e())
                // Reminder: unsafe_interval.e() == one.e()
                if (rest < unsafeIntervalF)
                {
                    // Rounding down (by not emitting the remaining digits) yields a number
                    // that lies within the unsafe interval.
                    too_high.Subtract(ref w);
                    return RoundWeed(buffer, length, too_high.F,
                                     unsafeIntervalF, rest,
                                     (ulong)(divisor) << -one.E, unit);
                }
                divisor /= 10;
            }

            // The integrals have been generated. We are at the point of the decimal
            // separator. In the following loop we simply multiply the remaining digits by
            // 10 and divide by one. We just need to pay attention to multiply associated
            // data (like the interval or 'unit'), too.
            // Note that the multiplication by 10 does not overflow, because w.e >= -60
            // and thus one.e >= -60.
            Debug.Assert(one.E >= -60);
            Debug.Assert(fractionals < one.F);
            Debug.Assert(0xFFFFFFFFFFFFFFFF / 10 >= one.F);
            while (true)
            {
                fractionals *= 10;
                unit *= 10;
                unsafe_interval.F *= 10;
                // Integer division by one.
                int digit = (int)(fractionals >> -one.E);
                buffer[length] = (char)('0' + digit);
                ++length;
                fractionals &= one.F - 1;  // Modulo by one.
                kappa--;
                if (fractionals < unsafe_interval.F)
                {
                    too_high.Subtract(ref w);
                    return RoundWeed(buffer, length, too_high.F * unit,
                                     unsafe_interval.F, fractionals, one.F, unit);
                }
            }
        }

        // Generates (at most) requested_digits digits of input number w.
        // w is a floating-point number (DiyFp), consisting of a significand and an
        // exponent. Its exponent is bounded by kMinimalTargetExponent and
        // kMaximalTargetExponent.
        //       Hence -60 <= w.e() <= -32.
        //
        // Returns false if it fails, in which case the generated digits in the buffer
        // should not be used.
        // Preconditions:
        //  * w is correct up to 1 ulp (unit in the last place). That
        //    is, its error must be strictly less than a unit of its last digit.
        //  * kMinimalTargetExponent <= w.e() <= kMaximalTargetExponent
        //
        // Postconditions: returns false if procedure fails.
        //   otherwise:
        //     * buffer is not null-terminated, but length contains the number of
        //       digits.
        //     * the representation in buffer is the most precise representation of
        //       requested_digits digits.
        //     * buffer contains at most requested_digits digits of w. If there are less
        //       than requested_digits digits then some trailing '0's have been removed.
        //     * kappa is such that
        //            w = buffer * 10^kappa + eps with |eps| < 10^kappa / 2.
        //
        // Remark: This procedure takes into account the imprecision of its input
        //   numbers. If the precision is not enough to guarantee all the postconditions
        //   then false is returned. This usually happens rarely, but the failure-rate
        //   increases with higher requested_digits.
        static bool DigitGenCounted(DiyFp w,
                                    int requested_digits,
                                    char[] buffer,
                                    out int length,
                                    out int kappa)
        {
            Debug.Assert(kMinimalTargetExponent <= w.E && w.E <= kMaximalTargetExponent);
            Debug.Assert(kMinimalTargetExponent >= -60);
            Debug.Assert(kMaximalTargetExponent <= -32);
            // w is assumed to have an error less than 1 unit. Whenever w is scaled we
            // also scale its error.
            ulong w_error = 1;
            // We cut the input number into two parts: the integral digits and the
            // fractional digits. We don't emit any decimal separator, but adapt kappa
            // instead. Example: instead of writing "1.2" we put "12" into the buffer and
            // increase kappa by 1.
            DiyFp one = new DiyFp(1UL << -w.E, w.E);
            // Division by one is a shift.
            uint integrals = (uint)(w.F >> -one.E);
            // Modulo by one is an and.
            ulong fractionals = w.F & (one.F - 1);
            uint divisor;
            int divisor_exponent_plus_one;
            BiggestPowerTen(integrals, DiyFp.kSignificandSize - (-one.E),
                            out divisor, out divisor_exponent_plus_one);
            kappa = divisor_exponent_plus_one;
            length = 0;

            // Loop invariant: buffer = w / 10^kappa  (integer division)
            // The invariant holds for the first iteration: kappa has been initialized
            // with the divisor exponent + 1. And the divisor is the biggest power of ten
            // that is smaller than 'integrals'.
            while (kappa > 0)
            {
                int digit = (int)(integrals / divisor);
                Debug.Assert(digit <= 9);
                buffer[length] = (char)('0' + digit);
                length++;
                requested_digits--;
                integrals %= divisor;
                kappa--;
                // Note that kappa now equals the exponent of the divisor and that the
                // invariant thus holds again.
                if (requested_digits == 0) break;
                divisor /= 10;
            }

            if (requested_digits == 0)
            {
                ulong rest =
                    ((ulong)(integrals) << -one.E) + fractionals;
                return RoundWeedCounted(buffer, length, rest,
                                        (ulong)(divisor) << -one.E, w_error,
                                        ref kappa);
            }

            // The integrals have been generated. We are at the point of the decimal
            // separator. In the following loop we simply multiply the remaining digits by
            // 10 and divide by one. We just need to pay attention to multiply associated
            // data (the 'unit'), too.
            // Note that the multiplication by 10 does not overflow, because w.e >= -60
            // and thus one.e >= -60.
            Debug.Assert(one.E >= -60);
            Debug.Assert(fractionals < one.F);
            Debug.Assert(0xFFFFFFFFFFFFFFFF / 10 >= one.F);
            while (requested_digits > 0 && fractionals > w_error)
            {
                fractionals *= 10;
                w_error *= 10;
                // Integer division by one.
                int digit = (int)(fractionals >> -one.E);
                Debug.Assert(digit <= 9);
                buffer[length] = (char)('0' + digit);
                length++;
                requested_digits--;
                fractionals &= one.F - 1;  // Modulo by one.
                kappa--;
            }
            if (requested_digits != 0) return false;
            return RoundWeedCounted(buffer, length, fractionals, one.F, w_error,
                                    ref kappa);
        }

        // Rounds the buffer upwards if the result is closer to v by possibly adding
        // 1 to the buffer. If the precision of the calculation is not sufficient to
        // round correctly, return false.
        // The rounding might shift the whole buffer in which case the kappa is
        // adjusted. For example "99", kappa = 3 might become "10", kappa = 4.
        //
        // If 2*rest > ten_kappa then the buffer needs to be round up.
        // rest can have an error of +/- 1 unit. This function accounts for the
        // imprecision and returns false, if the rounding direction cannot be
        // unambiguously determined.
        //
        // Precondition: rest < ten_kappa.
        static bool RoundWeedCounted(char[] buffer,
                                     int length,
                                     ulong rest,
                                     ulong ten_kappa,
                                     ulong unit,
                                     ref int kappa)
        {
            Debug.Assert(rest < ten_kappa);
            // The following tests are done in a specific order to avoid overflows. They
            // will work correctly with any uint64 values of rest < ten_kappa and unit.
            //
            // If the unit is too big, then we don't know which way to round. For example
            // a unit of 50 means that the real number lies within rest +/- 50. If
            // 10^kappa == 40 then there is no way to tell which way to round.
            if (unit >= ten_kappa) return false;
            // Even if unit is just half the size of 10^kappa we are already completely
            // lost. (And after the previous test we know that the expression will not
            // over/underflow.)
            if (ten_kappa - unit <= unit) return false;
            // If 2 * (rest + unit) <= 10^kappa we can safely round down.
            if ((ten_kappa - rest > rest) && (ten_kappa - 2 * rest >= 2 * unit))
            {
                return true;
            }
            // If 2 * (rest - unit) >= 10^kappa, then we can safely round up.
            if ((rest > unit) && (ten_kappa - (rest - unit) <= (rest - unit)))
            {
                // Increment the last digit recursively until we find a non '9' digit.
                buffer[length - 1]++;
                for (int i = length - 1; i > 0; --i)
                {
                    if (buffer[i] != '0' + 10) break;
                    buffer[i] = '0';
                    buffer[i - 1]++;
                }
                // If the first digit is now '0'+ 10 we had a buffer with all '9's. With the
                // exception of the first digit all digits are now '0'. Simply switch the
                // first digit to '1' and adjust the kappa. Example: "99" becomes "10" and
                // the power (the kappa) is increased.
                if (buffer[0] == '0' + 10)
                {
                    buffer[0] = '1';
                    kappa += 1;
                }
                return true;
            }
            return false;
        }

        // Returns the biggest power of ten that is less than or equal to the given
        // number. We furthermore receive the maximum number of bits 'number' has.
        //
        // Returns power == 10^(exponent_plus_one-1) such that
        //    power <= number < power * 10.
        // If number_bits == 0 then 0^(0-1) is returned.
        // The number of bits must be <= 32.
        // Precondition: number < (1 << (number_bits + 1)).

        // Inspired by the method for finding an integer log base 10 from here:
        // http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog10
        private static readonly uint[] kSmallPowersOfTen = new uint[]
        {
            0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000
        };

        static void BiggestPowerTen(uint number,
                                    int number_bits,
                                    out uint power,
                                    out int exponent_plus_one)
        {
            Debug.Assert(number < ((uint)(1) << (number_bits + 1)));
            // 1233/4096 is approximately 1/lg(10).
            int exponent_plus_one_guess = ((number_bits + 1) * 1233 >> 12);
            // We increment to skip over the first entry in the kPowersOf10 table.
            // Note: kPowersOf10[i] == 10^(i-1).
            exponent_plus_one_guess++;
            // We don't have any guarantees that 2^number_bits <= number.
            // TODO(floitsch): can we change the 'while' into an 'if'? We definitely see
            // number < (2^number_bits - 1), but I haven't encountered
            // number < (2^number_bits - 2) yet.
            while (number < kSmallPowersOfTen[exponent_plus_one_guess])
            {
                exponent_plus_one_guess--;
            }
            power = kSmallPowersOfTen[exponent_plus_one_guess];
            exponent_plus_one = exponent_plus_one_guess;
        }

        // Adjusts the last digit of the generated number, and screens out generated
        // solutions that may be inaccurate. A solution may be inaccurate if it is
        // outside the safe interval, or if we cannot prove that it is closer to the
        // input than a neighboring representation of the same length.
        //
        // Input: * buffer containing the digits of too_high / 10^kappa
        //        * the buffer's length
        //        * distance_too_high_w == (too_high - w).f() * unit
        //        * unsafe_interval == (too_high - too_low).f() * unit
        //        * rest = (too_high - buffer * 10^kappa).f() * unit
        //        * ten_kappa = 10^kappa * unit
        //        * unit = the common multiplier
        // Output: returns true if the buffer is guaranteed to contain the closest
        //    representable number to the input.
        //  Modifies the generated digits in the buffer to approach (round towards) w.
        static bool RoundWeed(char[] buffer,
                              int length,
                              ulong distance_too_high_w,
                              ulong unsafe_interval,
                              ulong rest,
                              ulong ten_kappa,
                              ulong unit)
        {
            ulong small_distance = distance_too_high_w - unit;
            ulong big_distance = distance_too_high_w + unit;
            // Let w_low  = too_high - big_distance, and
            //     w_high = too_high - small_distance.
            // Note: w_low < w < w_high
            //
            // The real w (* unit) must lie somewhere inside the interval
            // ]w_low; w_high[ (often written as "(w_low; w_high)")

            // Basically the buffer currently contains a number in the unsafe interval
            // ]too_low; too_high[ with too_low < w < too_high
            //
            //  too_high - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            //                     ^v 1 unit            ^      ^                 ^      ^
            //  boundary_high ---------------------     .      .                 .      .
            //                     ^v 1 unit            .      .                 .      .
            //   - - - - - - - - - - - - - - - - - - -  +  - - + - - - - - -     .      .
            //                                          .      .         ^       .      .
            //                                          .  big_distance  .       .      .
            //                                          .      .         .       .    rest
            //                              small_distance     .         .       .      .
            //                                          v      .         .       .      .
            //  w_high - - - - - - - - - - - - - - - - - -     .         .       .      .
            //                     ^v 1 unit                   .         .       .      .
            //  w ----------------------------------------     .         .       .      .
            //                     ^v 1 unit                   v         .       .      .
            //  w_low  - - - - - - - - - - - - - - - - - - - - -         .       .      .
            //                                                           .       .      v
            //  buffer --------------------------------------------------+-------+--------
            //                                                           .       .
            //                                                  safe_interval    .
            //                                                           v       .
            //   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     .
            //                     ^v 1 unit                                     .
            //  boundary_low -------------------------                     unsafe_interval
            //                     ^v 1 unit                                     v
            //  too_low  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            //
            //
            // Note that the value of buffer could lie anywhere inside the range too_low
            // to too_high.
            //
            // boundary_low, boundary_high and w are approximations of the real boundaries
            // and v (the input number). They are guaranteed to be precise up to one unit.
            // In fact the error is guaranteed to be strictly less than one unit.
            //
            // Anything that lies outside the unsafe interval is guaranteed not to round
            // to v when read again.
            // Anything that lies inside the safe interval is guaranteed to round to v
            // when read again.
            // If the number inside the buffer lies inside the unsafe interval but not
            // inside the safe interval then we simply do not know and bail out (returning
            // false).
            //
            // Similarly we have to take into account the imprecision of 'w' when finding
            // the closest representation of 'w'. If we have two potential
            // representations, and one is closer to both w_low and w_high, then we know
            // it is closer to the actual value v.
            //
            // By generating the digits of too_high we got the largest (closest to
            // too_high) buffer that is still in the unsafe interval. In the case where
            // w_high < buffer < too_high we try to decrement the buffer.
            // This way the buffer approaches (rounds towards) w.
            // There are 3 conditions that stop the decrementation process:
            //   1) the buffer is already below w_high
            //   2) decrementing the buffer would make it leave the unsafe interval
            //   3) decrementing the buffer would yield a number below w_high and farther
            //      away than the current number. In other words:
            //              (buffer{-1} < w_high) && w_high - buffer{-1} > buffer - w_high
            // Instead of using the buffer directly we use its distance to too_high.
            // Conceptually rest ~= too_high - buffer
            // We need to do the following tests in this order to avoid over- and
            // underflows.
            Debug.Assert(rest <= unsafe_interval);
            while (rest < small_distance &&  // Negated condition 1
                   unsafe_interval - rest >= ten_kappa &&  // Negated condition 2
                   (rest + ten_kappa < small_distance ||  // buffer{-1} > w_high
                    small_distance - rest >= rest + ten_kappa - small_distance))
            {
                buffer[length - 1]--;
                rest += ten_kappa;
            }

            // We have approached w+ as much as possible. We now test if approaching w-
            // would require changing the buffer. If yes, then we have two possible
            // representations close to w, but we cannot decide which one is closer.
            if (rest < big_distance &&
                unsafe_interval - rest >= ten_kappa &&
                (rest + ten_kappa < big_distance ||
                 big_distance - rest > rest + ten_kappa - big_distance))
            {
                return false;
            }

            // Weeding test.
            //   The safe interval is [too_low + 2 ulp; too_high - 2 ulp]
            //   Since too_low = too_high - unsafe_interval this is equivalent to
            //      [too_high - unsafe_interval + 4 ulp; too_high - 2 ulp]
            //   Conceptually we have: rest ~= too_high - buffer
            return (2 * unit <= rest) && (rest <= unsafe_interval - 4 * unit);
        }

        private static void CreateDecimalRepresentation(
            char[] decimal_digits,
            int length,
            int decimal_point,
            int digits_after_point,
            TextWriter writer)
        {
            // Create a representation that is padded with zeros if needed.
            if (decimal_point <= 0)
            {
                // "0.00000decimal_rep".
                writer.Write('0');
                if (digits_after_point > 0)
                {
                    writer.Write('.');
                    writer.Write(new string('0', -decimal_point));
                    Debug.Assert(length <= digits_after_point - (-decimal_point));
                    writer.Write(decimal_digits, 0, length);
                    int remaining_digits = digits_after_point - (-decimal_point) - length;
                    writer.Write(new string('0', remaining_digits));
                }
            }
            else if (decimal_point >= length)
            {
                // "decimal_rep0000.00000" or "decimal_rep.0000"
                writer.Write(decimal_digits, 0, length);
                writer.Write(new string('0', decimal_point - length));
                if (digits_after_point > 0)
                {
                    writer.Write('.');
                    writer.Write(new string('0', digits_after_point));
                }
            }
            else
            {
                // "decima.l_rep000"
                Debug.Assert(digits_after_point > 0);
                writer.Write(decimal_digits, 0, decimal_point);
                writer.Write('.');
                Debug.Assert(length - decimal_point <= digits_after_point);
                writer.Write(decimal_digits, decimal_point,
                                             length - decimal_point);
                int remaining_digits = digits_after_point - (length - decimal_point);
                writer.Write(new string('0', remaining_digits));
            }
        }

        private static void CreateExponentialRepresentation(
            char[] decimal_digits,
            int length,
            int exponent,
            TextWriter writer)
        {
            Debug.Assert(length != 0);
            writer.Write(decimal_digits[0]);
            if (length != 1)
            {
                writer.Write('.');
                writer.Write(decimal_digits, 1, length - 1);
            }
            writer.Write(exponent_character_);
            if (exponent < 0)
            {
                writer.Write('-');
                exponent = -exponent;
            }
            if (exponent == 0)
            {
                writer.Write('0');
                return;
            }
            Debug.Assert(exponent < 1e4);
            if (exponent >= 100)
            {
                writer.Write((char)('0' + exponent / 100));
                exponent %= 100;
                writer.Write((char)('0' + exponent / 10));
                exponent %= 10;
                writer.Write((char)('0' + exponent));
            }
            else if (exponent >= 10)
            {
                writer.Write((char)('0' + exponent / 10));
                exponent %= 10;
                writer.Write((char)('0' + exponent));
            }
            else
            {
                writer.Write(exponent);
            }
        }
    }
}
