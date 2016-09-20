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
    }
}
