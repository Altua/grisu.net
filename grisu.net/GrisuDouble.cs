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

namespace GrisuDotNet
{
    internal struct GrisuDouble
    {
        private const ulong kSignMask = 0x8000000000000000;
        private const ulong kExponentMask = 0x7FF0000000000000;
        private const ulong kSignificandMask = 0x000FFFFFFFFFFFFF;
        private const ulong kHiddenBit = 0x0010000000000000;
        private const int kPhysicalSignificandSize = 52;  // Excludes the hidden bit.
        private const int kSignificandSize = 53;
        private const int kExponentBias = 0x3FF + kPhysicalSignificandSize;
        private const int kDenormalExponent = -kExponentBias + 1;
        private const int kMaxExponent = 0x7FF - kExponentBias;
        private const ulong kInfinity = 0x7FF0000000000000;

        public GrisuDouble(double d)
        {
            value_ = d;
            d64_ = (ulong)BitConverter.DoubleToInt64Bits(d);
        }

        public GrisuDouble(ulong d64)
        {
            d64_ = d64;
            value_ = BitConverter.Int64BitsToDouble((long)d64);
        }

        public GrisuDouble(DiyFp diy_fp)
        {
            d64_ = DiyFpToUInt64(diy_fp);
            value_ = BitConverter.Int64BitsToDouble((long)d64_);
        }

        // The value encoded by this Double must be greater or equal to +0.0.
        // It must not be special (infinity, or NaN).
        public DiyFp AsDiyFp()
        {
            Debug.Assert(Sign > 0);
            Debug.Assert(!IsSpecial);
            return new DiyFp(Significand, Exponent);
        }

        // The value encoded by this Double must be strictly greater than 0.
        public DiyFp AsNormalizedDiyFp()
        {
            Debug.Assert(Value > 0.0);

            ulong d64 = d64_;
            ulong f;
            int e;
            if (IsDenormal)
            {
                f = d64 & kSignificandMask;
                e = kDenormalExponent;
            }
            else
            {
                f = (d64 & kSignificandMask) + kHiddenBit;
                e = (int)((d64 & kExponentMask) >> kPhysicalSignificandSize) - kExponentBias;
            }

            // The current double could be a denormal.
            while ((f & kHiddenBit) == 0)
            {
                f <<= 1;
                e--;
            }
            // Do the final shifts in one go.
            f <<= DiyFp.kSignificandSize - kSignificandSize;
            e -= DiyFp.kSignificandSize - kSignificandSize;
            return new DiyFp(f, e);
        }

        // Returns the double's bit as UInt64.
        public ulong AsUInt64()
        {
            return d64_;
        }

        // Returns an estimation of k such that 10^(k-1) <= v < 10^k where
        // v = f * 2^exponent and 2^52 <= f < 2^53.
        // v is hence a normalized double with the given exponent. The output is an
        // approximation for the exponent of the decimal approimation .digits * 10^k.
        //
        // The result might undershoot by 1 in which case 10^k <= v < 10^k+1.
        // Note: this property holds for v's upper boundary m+ too.
        //    10^k <= m+ < 10^k+1.
        //   (see explanation below).
        //
        // Examples:
        //  EstimatePower(0)   => 16
        //  EstimatePower(-52) => 0
        //
        // Note: e >= 0 => EstimatedPower(e) > 0. No similar claim can be made for e<0.
        internal static int EstimatePower(int exponent)
        {
            // This function estimates log10 of v where v = f*2^e (with e == exponent).
            // Note that 10^floor(log10(v)) <= v, but v <= 10^ceil(log10(v)).
            // Note that f is bounded by its container size. Let p = 53 (the double's
            // significand size). Then 2^(p-1) <= f < 2^p.
            //
            // Given that log10(v) == log2(v)/log2(10) and e+(len(f)-1) is quite close
            // to log2(v) the function is simplified to (e+(len(f)-1)/log2(10)).
            // The computed number undershoots by less than 0.631 (when we compute log3
            // and not log10).
            //
            // Optimization: since we only need an approximated result this computation
            // can be performed on 64 bit integers. On x86/x64 architecture the speedup is
            // not really measurable, though.
            //
            // Since we want to avoid overshooting we decrement by 1e10 so that
            // floating-point imprecisions don't affect us.
            //
            // Explanation for v's boundary m+: the computation takes advantage of
            // the fact that 2^(p-1) <= f < 2^p. Boundaries still satisfy this requirement
            // (even for denormals where the delta can be much more important).

            // For doubles len(f) == 53 (don't forget the hidden bit).
            double estimate = Math.Ceiling((exponent + kSignificandSize - 1) * k1Log10 - 1e-10);
            return (int)estimate;
        }

        private const double k1Log10 = 0.30102999566398114;  // 1/lg(10)

        public static int NormalizedExponent(ulong significand, int exponent)
        {
            Debug.Assert(significand != 0);
            while ((significand & kHiddenBit) == 0)
            {
                significand = significand << 1;
                exponent = exponent - 1;
            }
            return exponent;
        }

        public bool LowerBoundaryIsCloser() {
            // The boundary is closer if the significand is of the form f == 2^p-1 then
            // the lower boundary is closer.
            // Think of v = 1000e10 and v- = 9999e9.
            // Then the boundary (== (v - v-)/2) is not just at a distance of 1e9 but
            // at a distance of 1e8.
            // The only exception is for the smallest normal: the largest denormal is
            // at the same distance as its successor.
            // Note: denormals have the same exponent as the smallest normals.
            bool physical_significand_is_zero = ((AsUInt64() & kSignificandMask) == 0);
            return physical_significand_is_zero && (Exponent != kDenormalExponent);
          }

    public int Exponent
        {
            get
            {
                if (IsDenormal) return kDenormalExponent;

                int biased_e =
                    (int)((d64_ & kExponentMask) >> kPhysicalSignificandSize);
                return biased_e - kExponentBias;
            }
        }

        public ulong Significand
        {
            get
            {
                ulong significand = d64_ & kSignificandMask;
                if (IsDenormal)
                {
                    return significand;
                    
                }
                else
                {
                    return significand + kHiddenBit;
                }
            }
        }

        // Returns true if the double is a denormal.
        public bool IsDenormal => (d64_ & kExponentMask) == 0;

        // We consider denormals not to be special.
        // Hence only Infinity and NaN are special.
        public bool IsSpecial => (d64_ & kExponentMask) == kExponentMask;

        public bool IsNaN => ((d64_ & kExponentMask) == kExponentMask) &&
                             ((d64_ & kSignificandMask) != 0);

        public bool IsInfinite => ((d64_ & kExponentMask) == kExponentMask) &&
                                  ((d64_ & kSignificandMask) == 0);

        public int Sign => (d64_ & kSignMask) == 0 ? 1 : -1;

        // Precondition: the value encoded by this Double must be greater or equal
        // than +0.0.
        public DiyFp UpperBoundary()
        {
            Debug.Assert(Sign > 0);
            return new DiyFp(Significand * 2 + 1, Exponent - 1);
        }

        // Computes the two boundaries of this.
        // The bigger boundary (m_plus) is normalized. The lower boundary has the same
        // exponent as m_plus.
        // Precondition: the value encoded by this Double must be greater than 0.
        public void NormalizedBoundaries(out DiyFp out_m_minus, out DiyFp out_m_plus)
        {
            Debug.Assert(Value > 0.0);

            ulong d64 = d64_;
            ulong vF;
            int vE;
            if (IsDenormal)
            {
                vF = d64 & kSignificandMask;
                vE = kDenormalExponent;
            }
            else
            {
                vF = (d64 & kSignificandMask) + kHiddenBit;
                vE = (int)((d64 & kExponentMask) >> kPhysicalSignificandSize) - kExponentBias;
            }

            ulong plusF = (vF << 1) + 1;
            int plusE = vE - 1;

            // This code is manually inlined from the GrisuDouble.Normalize() method,
            // because the .NET JIT (at least the 64-bit one as of version 4) is too
            // incompetent to do it itself.
            const ulong k10MSBits = 0xFFC0000000000000;
            const ulong kUint64MSB = 0x8000000000000000;
            while ((plusF & k10MSBits) == 0)
            {
                plusF <<= 10;
                plusE -= 10;
            }
            while ((plusF & kUint64MSB) == 0)
            {
                plusF <<= 1;
                plusE--;
            }

            ulong minusF;
            int minusE;
            bool significand_is_zero = (vF == kHiddenBit);
            if (significand_is_zero && vE != kDenormalExponent)
            {
                // The boundary is closer. Think of v = 1000e10 and v- = 9999e9.
                // Then the boundary (== (v - v-)/2) is not just at a distance of 1e9 but
                // at a distance of 1e8.
                // The only exception is for the smallest normal: the largest denormal is
                // at the same distance as its successor.
                // Note: denormals have the same exponent as the smallest normals.
                minusF = (vF << 2) - 1;
                minusE = vE - 2;
            }
            else
            {
                minusF = (vF << 1) - 1;
                minusE = vE - 1;
            }
            out_m_minus = new DiyFp(minusF << (minusE - plusE), plusE);
            out_m_plus = new DiyFp(plusF, plusE);
        }

        public double Value => value_;

        // Returns the significand size for a given order of magnitude.
        // If v = f*2^e with 2^p-1 <= f <= 2^p then p+e is v's order of magnitude.
        // This function returns the number of significant binary digits v will have
        // once it's encoded into a double. In almost all cases this is equal to
        // kSignificandSize. The only exceptions are denormals. They start with
        // leading zeroes and their effective significand-size is hence smaller.
        public static int SignificandSizeForOrderOfMagnitude(int order)
        {
            if (order >= (kDenormalExponent + kSignificandSize))
            {
                return kSignificandSize;
            }
            if (order <= kDenormalExponent) return 0;
            return order - kDenormalExponent;
        }

        public static double Infinity => double.PositiveInfinity;

        public static double NaN => double.NaN;

        private static ulong DiyFpToUInt64(DiyFp diy_fp)
        {
            ulong significand = diy_fp.F;
            int exponent = diy_fp.E;
            while (significand > kHiddenBit + kSignificandMask)
            {
                significand >>= 1;
                exponent++;
            }
            if (exponent >= kMaxExponent)
            {
                return kInfinity;
            }
            if (exponent < kDenormalExponent)
            {
                return 0;
            }
            while (exponent > kDenormalExponent && (significand & kHiddenBit) == 0)
            {
                significand <<= 1;
                exponent--;
            }
            ulong biased_exponent;
            if (exponent == kDenormalExponent && (significand & kHiddenBit) == 0)
            {
                biased_exponent = 0;
            }
            else
            {
                biased_exponent = (ulong)(exponent + kExponentBias);
            }
            return (significand & kSignificandMask) |
                (biased_exponent << kPhysicalSignificandSize);
        }

        private readonly ulong d64_;
        private readonly double value_;
    }
}
