/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:41:20 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.OpenMapRealVector;
import org.apache.commons.math.linear.RealVector;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class OpenMapRealVector_ESTest extends OpenMapRealVector_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double[] doubleArray0 = new double[8];
      doubleArray0[0] = 1.0;
      doubleArray0[1] = (-2270.1471571883353);
      doubleArray0[2] = 415.0938092106;
      doubleArray0[3] = (-1513.98588087074);
      doubleArray0[4] = 415.0938092106;
      doubleArray0[5] = 1.633123935319537E16;
      doubleArray0[6] = 1.0E-12;
      doubleArray0[7] = 0.0;
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector(doubleArray0, 415.0938092106);
      openMapRealVector0.isDefaultValue((-2270.1471571883353));
      openMapRealVector0.unitize();
      RealVector realVector0 = openMapRealVector0.mapMultiply((-1513.98588087074));
      // Undeclared exception!
      openMapRealVector0.ebeMultiply(realVector0);
  }
}
