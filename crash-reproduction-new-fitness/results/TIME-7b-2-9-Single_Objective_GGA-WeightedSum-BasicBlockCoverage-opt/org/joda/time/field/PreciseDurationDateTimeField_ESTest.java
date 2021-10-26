/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:36:30 UTC 2021
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
import org.joda.time.field.TestPreciseDurationDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PreciseDurationDateTimeField_ESTest extends PreciseDurationDateTimeField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeFieldType.hourOfDay();
      DurationField durationField0 = mock(DurationField.class, new ViolatedAssumptionAnswer());
      TestPreciseDurationDateTimeField.MockStandardBaseDateTimeField testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0 = new TestPreciseDurationDateTimeField.MockStandardBaseDateTimeField();
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.getDifferenceAsLong(0L, 0L);
      int int0 = 0;
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.getMaximumValueForSet((-1250L), 0);
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.getUnitMillis();
      int int1 = 1971;
      // Undeclared exception!
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.set((-1250L), 1971);
  }
}
