using System;
using System.Text;
using Altua.UnitTesting;
using GrisuDotNet;
using Xunit;

namespace Tests
{
    public sealed class GrisuUnitTests
    {
        [Theory]
        [InlineData(0.0, new [] {'0'})]
        [InlineData(0.000123456789123456, new[] { '1', '2', '3','4','5','6','7','8','9','1','2','3','4','5','6' })]
        [InlineData(12345.0, new[] { '1','2','3','4','5'})]
        [InlineData(12345e23, new[] { '1', '2', '3', '4', '5' })]
        [InlineData(0.0, new[] { '0' })]
        [InlineData(1e21, new[] { '1' })]
        [InlineData(1e20, new[] { '1' })]
        [InlineData(111111111111111111111.0, new[] { '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1' })]
        [InlineData(1111111111111111111111.0, new[] {'1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1','1'})]
        [InlineData(11111111111111111111111.0, new[] { '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1' })]
        [InlineData(-0.001, new[] { '1' })]
        [InlineData(-0.0001, new[] { '1' })]
        [InlineData(-0.00001, new[] { '1' })]
        [InlineData(-0.000001, new[] { '1' })]
        [InlineData(-0.0000001, new[] { '1' })]
        [InlineData(-0.0, new[] { '0'})]
        [InlineData(0.1, new[] { '1' })]
        [InlineData(0.01, new[] { '1' })]
        [InlineData(1.0, new[] { '1' })]
        [InlineData(10.0, new[] { '1' })]
        [InlineData(1100.0, new[] { '1' , '1' })]
        [InlineData(1122.0, new[] { '1' , '1' , '2' , '2' })]
        [InlineData(10000.0, new[] { '1' })]
        [InlineData(11100.0, new[] { '1' , '1' , '1' })]
        [InlineData(100000.0, new[] { '1' })]
        [InlineData(0.000001, new[] { '1' })]
        [InlineData(0.0000001, new[] { '1' })]
        [InlineData(100000000000000000000.0, new[] { '1' })]
        [InlineData(3.5844466002796428e+298, new[] { '3','5','8','4','4','4','6','6','0','0','2','7','9','6','4','2','8' })]
        public void Grisu_WithShortest_DoubleToAscii_ReturnsExpectedBuffer(double value, char[] expected)
        {
            int length, point;
            bool sign;
            char[] buffer = new char[Grisu.kBase10MaximalLength];

            Grisu.DoubleToAscii(value, DtoaMode.SHORTEST, -1, ref buffer, out sign, out length, out point);

            char[] actual = new char[length];
            Array.Copy(buffer, 0, actual, 0, length);
            AssertSequence.Equal(expected, actual);
        }


        [Theory]
        [InlineData(0.0, false)]
        [InlineData(0.000123456789123456, false)]
        [InlineData(12345.0, false)]
        [InlineData(12345e23, false)]
        [InlineData(0.0, false)]
        [InlineData(1e21, false)]
        [InlineData(1e20, false)]
        [InlineData(111111111111111111111.0, false)]
        [InlineData(1111111111111111111111.0, false)]
        [InlineData(11111111111111111111111.0, false)]
        [InlineData(-0.001, true)]
        [InlineData(-0.0001, true)]
        [InlineData(-0.00001, true)]
        [InlineData(-0.000001, true)]
        [InlineData(-0.0000001, true)]
        [InlineData(-45.87897946, true)]
        [InlineData(0.1, false)]
        [InlineData(0.01, false)]
        [InlineData(1.0, false)]
        [InlineData(10.0, false)]
        [InlineData(1100.0, false)]
        [InlineData(1122.0, false)]
        [InlineData(10000.0, false)]
        [InlineData(11100.0, false)]
        [InlineData(100000.0, false)]
        [InlineData(0.000001, false)]
        [InlineData(0.0000001, false)]
        [InlineData(100000000000000000000.0, false)]
        [InlineData(3.5844466002796428e+298, false)]
        public void Grisu_WithShortest_DoubleToAscii_ReturnsExpectedSign(double value, bool expected)
        {
            int length, point;
            bool actual;
            char[] buffer = new char[Grisu.kBase10MaximalLength];

            Grisu.DoubleToAscii(value, DtoaMode.SHORTEST, -1, ref buffer, out actual, out length, out point);

            Assert.Equal(expected, actual);
        }


        [Theory]
        [InlineData(0.0, 1)]
        [InlineData(0.000123456789123456, 15)]
        [InlineData(12345.0, 5)]
        [InlineData(12345e23, 5)]
        [InlineData(0.0, 1)]
        [InlineData(1e21, 1)]
        [InlineData(1e20, 1)]
        [InlineData(111111111111111111111.0, 21)]
        [InlineData(1111111111111111111111.0, 22)]
        [InlineData(11111111111111111111111.0, 23)]
        [InlineData(-0.001, 1)]
        [InlineData(-0.0001, 1)]
        [InlineData(-0.00001, 1)]
        [InlineData(-0.000001, 1)]
        [InlineData(-0.0000001, 1)]
        [InlineData(-0.0, 1)]
        [InlineData(0.1, 1)]
        [InlineData(0.01, 1)]
        [InlineData(1.0, 1)]
        [InlineData(10.0, 1)]
        [InlineData(1100.0, 2)]
        [InlineData(1122.0, 4)]
        [InlineData(10000.0, 1)]
        [InlineData(11100.0, 3)]
        [InlineData(100000.0, 1)]
        [InlineData(0.000001, 1)]
        [InlineData(0.0000001, 1)]
        [InlineData(100000000000000000000.0, 1)]
        [InlineData(3.5844466002796428e+298, 17)]
        public void Grisu_WithShortest_DoubleToAscii_ReturnsExpectedLength(double value, int expected)
        {
            int actual, point;
            bool sign;
            char[] buffer = new char[Grisu.kBase10MaximalLength];

            Grisu.DoubleToAscii(value, DtoaMode.SHORTEST, -1, ref buffer, out sign, out actual, out point);

            Assert.Equal(expected, actual);
        }


        [Theory]
        [InlineData(0.0, 1)]
        [InlineData(0.000123456789123456, -3)]
        [InlineData(12345.0, 5)]
        [InlineData(12345e23, 28)]
        [InlineData(0.0, 1)]
        [InlineData(1e21, 22)]
        [InlineData(1e20, 21)]
        [InlineData(111111111111111111111.0, 21)]
        [InlineData(1111111111111111111111.0, 22)]
        [InlineData(11111111111111111111111.0, 23)]
        [InlineData(-0.001, -2)]
        [InlineData(-0.0001, -3)]
        [InlineData(-0.00001, -4)]
        [InlineData(-0.000001, -5)]
        [InlineData(-0.0000001, -6)]
        [InlineData(-0.0, 1)]
        [InlineData(0.1, 0)]
        [InlineData(0.01, -1)]
        [InlineData(1.0, 1)]
        [InlineData(10.0, 2)]
        [InlineData(1100.0, 4)]
        [InlineData(1122.0, 4)]
        [InlineData(10000.0, 5)]
        [InlineData(11100.0, 5)]
        [InlineData(100000.0, 6)]
        [InlineData(0.000001, -5)]
        [InlineData(0.0000001, -6)]
        [InlineData(100000000000000000000.0, 21)]
        [InlineData(3.5844466002796428e+298, 299)]
        public void Grisu_WithShortest_DoubleToAscii_ReturnsExpectedDecimalPoint(double value, int expected)
        {
            int length, actual;
            bool sign;
            char[] buffer = new char[Grisu.kBase10MaximalLength];

            Grisu.DoubleToAscii(value, DtoaMode.SHORTEST, -1, ref buffer, out sign, out length, out actual);

            Assert.Equal(expected, actual);
        }
    }
}
