/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 04:40:45 UTC 2020
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
import org.joda.time.field.MillisDurationField;
import org.joda.time.field.UnsupportedDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.clockhourOfDay();
      MillisDurationField millisDurationField0 = (MillisDurationField)MillisDurationField.INSTANCE;
      UnsupportedDateTimeField unsupportedDateTimeField0 = UnsupportedDateTimeField.getInstance(dateTimeFieldType0, millisDurationField0);
      int int0 = 33;
      FieldUtils.verifyValueBounds((DateTimeField) unsupportedDateTimeField0, 33, 33, 33);
      FieldUtils.safeMultiply((long) 33, 1245L);
      FieldUtils.safeNegate(1180);
      int int1 = 0;
      FieldUtils.safeMultiply(1245L, 0);
      int int2 = 0;
      FieldUtils.getWrappedValue(33, (-1180), 0);
      int int3 = (-538);
      // Undeclared exception!
      FieldUtils.verifyValueBounds((DateTimeField) unsupportedDateTimeField0, (-538), 33, 0);
  }
}
