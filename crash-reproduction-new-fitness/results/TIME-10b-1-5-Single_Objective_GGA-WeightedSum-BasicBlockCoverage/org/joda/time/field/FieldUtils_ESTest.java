/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 23:40:42 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.field.FieldUtils;
import org.joda.time.field.OffsetDateTimeField;
import org.joda.time.field.TestPreciseDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TestPreciseDateTimeField.MockPreciseDateTimeField testPreciseDateTimeField_MockPreciseDateTimeField0 = new TestPreciseDateTimeField.MockPreciseDateTimeField();
      int int0 = 1545;
      OffsetDateTimeField offsetDateTimeField0 = new OffsetDateTimeField(testPreciseDateTimeField_MockPreciseDateTimeField0, 1545);
      // Undeclared exception!
      FieldUtils.verifyValueBounds((DateTimeField) offsetDateTimeField0, (-2047), 0, (-2047));
  }
}
