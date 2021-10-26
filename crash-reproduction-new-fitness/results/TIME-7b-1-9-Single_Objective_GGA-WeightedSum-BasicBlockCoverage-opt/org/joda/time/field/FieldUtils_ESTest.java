/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:36:27 UTC 2021
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.field.FieldUtils;
import org.joda.time.field.TestPreciseDurationDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      long long0 = (-7877L);
      FieldUtils.safeAdd((-7877L), (-7877L));
      TestPreciseDurationDateTimeField.MockPreciseDurationDateTimeField testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0 = new TestPreciseDurationDateTimeField.MockPreciseDurationDateTimeField();
      int int0 = 1968;
      int int1 = 22;
      // Undeclared exception!
      FieldUtils.verifyValueBounds((DateTimeField) testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0, 1968, 1968, 22);
  }
}
