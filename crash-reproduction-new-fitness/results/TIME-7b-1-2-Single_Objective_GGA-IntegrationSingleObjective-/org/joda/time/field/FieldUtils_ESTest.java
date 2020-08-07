/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 13:19:06 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.field.FieldUtils;
import org.joda.time.field.TestOffsetDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      FieldUtils.safeAdd(669, 669);
      FieldUtils.safeAdd(0L, 2135L);
      TestOffsetDateTimeField.MockOffsetDateTimeField testOffsetDateTimeField_MockOffsetDateTimeField0 = new TestOffsetDateTimeField.MockOffsetDateTimeField();
      int int0 = 0;
      // Undeclared exception!
      FieldUtils.verifyValueBounds((DateTimeField) testOffsetDateTimeField_MockOffsetDateTimeField0, (-518), 0, 0);
  }
}
