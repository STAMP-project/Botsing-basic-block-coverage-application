/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 23:43:09 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.joda.time.DateTimeFieldType;
import org.joda.time.DurationField;
import org.joda.time.field.TestBaseDateTimeField;
import org.joda.time.field.TestPreciseDurationDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PreciseDurationDateTimeField_ESTest extends PreciseDurationDateTimeField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeFieldType dateTimeFieldType0 = mock(DateTimeFieldType.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null).when(dateTimeFieldType0).getName();
      TestBaseDateTimeField.MockStandardBaseDateTimeField testBaseDateTimeField_MockStandardBaseDateTimeField0 = new TestBaseDateTimeField.MockStandardBaseDateTimeField();
      DurationField durationField0 = testBaseDateTimeField_MockStandardBaseDateTimeField0.getRangeDurationField();
      TestPreciseDurationDateTimeField.MockPreciseDurationDateTimeField testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0 = new TestPreciseDurationDateTimeField.MockPreciseDurationDateTimeField(dateTimeFieldType0, durationField0);
      testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0.getMaximumValue();
      testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0.getUnitMillis();
      // Undeclared exception!
      testPreciseDurationDateTimeField_MockPreciseDurationDateTimeField0.set(1L, (-2804));
  }
}
