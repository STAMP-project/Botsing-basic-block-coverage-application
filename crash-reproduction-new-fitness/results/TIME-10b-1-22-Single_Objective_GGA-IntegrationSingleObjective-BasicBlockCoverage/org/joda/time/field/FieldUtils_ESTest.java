/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 04:42:32 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.DateTimeFieldType;
import org.joda.time.field.FieldUtils;
import org.joda.time.field.OffsetDateTimeField;
import org.joda.time.field.TestBaseDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      FieldUtils.safeMultiply((-324L), (-994));
      FieldUtils.safeMultiply(876, (-994));
      int int0 = 4200;
      FieldUtils.verifyValueBounds((DateTimeFieldType) null, 0, (-870744), 4200);
      FieldUtils.safeMultiply((-870744), (-1538));
      FieldUtils.equals((Object) null, (Object) null);
      TestBaseDateTimeField.MockStandardBaseDateTimeField testBaseDateTimeField_MockStandardBaseDateTimeField0 = new TestBaseDateTimeField.MockStandardBaseDateTimeField();
      OffsetDateTimeField offsetDateTimeField0 = new OffsetDateTimeField(testBaseDateTimeField_MockStandardBaseDateTimeField0, 876);
      // Undeclared exception!
      FieldUtils.verifyValueBounds((DateTimeField) offsetDateTimeField0, (-870744), (-251), 505);
  }
}
