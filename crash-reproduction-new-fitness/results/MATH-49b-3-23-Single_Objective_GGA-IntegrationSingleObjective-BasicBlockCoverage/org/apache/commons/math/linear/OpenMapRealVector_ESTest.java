/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:37:21 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.linear.OpenMapRealVector;
import org.apache.commons.math.linear.RealVector;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class OpenMapRealVector_ESTest extends OpenMapRealVector_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Double[] doubleArray0 = new Double[3];
      Double double0 = new Double(0.0);
      doubleArray0[0] = double0;
      double double1 = 2178.57;
      Double double2 = new Double(2178.57);
      doubleArray0[1] = double2;
      Double double3 = new Double((double) doubleArray0[1]);
      doubleArray0[2] = double3;
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector(doubleArray0);
      openMapRealVector0.hashCode();
      UnivariateRealFunction univariateRealFunction0 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0, 0.0).when(univariateRealFunction0).value(anyDouble());
      RealVector realVector0 = openMapRealVector0.map(univariateRealFunction0);
      openMapRealVector0.mapMultiply(0.0);
      // Undeclared exception!
      openMapRealVector0.ebeMultiply(realVector0);
  }
}
