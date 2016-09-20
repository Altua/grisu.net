// Copyright Altua AS. All rights reserved.

using System.Collections.Generic;
using System.Linq;
using Xunit.Sdk;


namespace Altua.UnitTesting
{
    public static class AssertSequence
    {
        public static void Equal<T>(T[] expected, T[] actual)
        {
            if (ReferenceEquals(expected, null))
            {
                string message = $"{nameof(expected)} can not be 'null'";
                throw new AssertActualExpectedException(null, actual, message);
            }

            if (ReferenceEquals(actual, null))
            {
                string message = $"{nameof(actual)} can not be 'null'";
                throw new AssertActualExpectedException(expected, null, message);
            }

            if (ReferenceEquals(expected, actual))
                return;

            if (expected.Length != actual.Length)
            {
                string message =
                    $"{nameof(expected)} length of '{expected.Length}' is different from {nameof(actual)} length of '{actual.Length}'";
                throw new AssertActualExpectedException(expected, actual, message);
            }

            if (expected.SequenceEqual(actual))
                return;

            List<int> diffIndices = new List<int>();
            for (int i = 0; i < expected.Length; i++)
            {
                if (!expected[i].Equals(actual[i]))
                {
                    diffIndices.Add(i);
                }
            }

            if (diffIndices.Count > 0)
            {
                string message = "The arrays contain different items at the following indices: ";
                message = message + string.Join(", ", diffIndices);
                throw new AssertActualExpectedException(expected, actual, message);
            }
        }
    }
}
