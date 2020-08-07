/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:45:06 UTC 2020
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
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector(886);
      UnivariateRealFunction univariateRealFunction0 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(univariateRealFunction0).value(anyDouble());
      RealVector realVector0 = openMapRealVector0.mapToSelf(univariateRealFunction0);
      openMapRealVector0.toArray();
      RealVector realVector1 = openMapRealVector0.mapSubtract(886);
      OpenMapRealVector openMapRealVector1 = openMapRealVector0.subtract(realVector1);
      openMapRealVector0.isInfinite();
      OpenMapRealVector openMapRealVector2 = new OpenMapRealVector(openMapRealVector1);
      // Undeclared exception!
      openMapRealVector2.ebeMultiply(realVector0);
  }
}
