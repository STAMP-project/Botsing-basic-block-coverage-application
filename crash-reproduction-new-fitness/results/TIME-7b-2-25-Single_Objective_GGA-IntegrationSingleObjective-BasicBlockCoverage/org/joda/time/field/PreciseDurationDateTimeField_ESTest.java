/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 17:46:24 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.field.TestPreciseDurationDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PreciseDurationDateTimeField_ESTest extends PreciseDurationDateTimeField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TestPreciseDurationDateTimeField.MockPreciseDurationDateTimeField testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0 = new TestPreciseDurationDateTimeField.MockPreciseDurationDateTimeField();
      testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0.addWrapField(216L, 3661);
      testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0.set(0L, 58);
      testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0.getType();
      // Undeclared exception!
      testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0.set((-123L), 3661);
  }
}
