using System.Diagnostics;

namespace Tests
{
    public struct PrecomputedShortest
    {
        public PrecomputedShortest(double value, string representation, int decimalPoint)
        {
            Value = value;
            Representation = representation;
            DecimalPoint = decimalPoint;
        }

        public double Value { get; }
        public string Representation { get; }
        public int DecimalPoint { get; }

        public override string ToString()
        {
            return string.Join(", ", "PrecomputedShortest", Value, Representation, DecimalPoint);
        }
    }
}
