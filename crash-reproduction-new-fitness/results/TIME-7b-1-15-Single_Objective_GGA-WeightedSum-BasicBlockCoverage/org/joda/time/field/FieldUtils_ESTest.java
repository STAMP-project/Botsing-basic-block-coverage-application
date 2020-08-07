/*
 * This file was automatically generated by EvoSuite
 * Fri May 15 12:24:04 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.field.FieldUtils;
import org.joda.time.field.TestPreciseDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TestPreciseDateTimeField.MockPreciseDateTimeField testPreciseDateTimeField_MockPreciseDateTimeField0 = new TestPreciseDateTimeField.MockPreciseDateTimeField();
      FieldUtils.verifyValueBounds((DateTimeField) testPreciseDateTimeField_MockPreciseDateTimeField0, (-3178), (-3178), 0);
      FieldUtils.safeMultiply(1L, 3150);
      long long0 = 1000000000000000L;
      FieldUtils.safeMultiply(1000000000000000L, 3150);
      FieldUtils.safeMultiplyToInt(4L, (-3178));
      FieldUtils.safeNegate(0);
      // Undeclared exception!
      FieldUtils.verifyValueBounds((DateTimeField) testPreciseDateTimeField_MockPreciseDateTimeField0, (-3178), 0, 0);
  }
}
