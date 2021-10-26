/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:36:35 UTC 2021
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.joda.time.DateTimeFieldType;
import org.joda.time.field.TestPreciseDateTimeField;
import org.joda.time.field.TestPreciseDurationDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PreciseDurationDateTimeField_ESTest extends PreciseDurationDateTimeField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeFieldType dateTimeFieldType0 = mock(DateTimeFieldType.class, new ViolatedAssumptionAnswer());
      DateTimeFieldType dateTimeFieldType1 = mock(DateTimeFieldType.class, new ViolatedAssumptionAnswer());
      int[] intArray0 = new int[6];
      Locale locale0 = new Locale("", "", "Unit duration field must be preise");
      locale0.stripExtensions();
      TestPreciseDurationDateTimeField.MockStandardBaseDateTimeField testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0 = new TestPreciseDurationDateTimeField.MockStandardBaseDateTimeField();
      locale0.clone();
      TestPreciseDateTimeField.MockStandardDateTimeField testPreciseDateTimeField_MockStandardDateTimeField0 = new TestPreciseDateTimeField.MockStandardDateTimeField();
      testPreciseDateTimeField_MockStandardDateTimeField0.getMinimumValue();
      testPreciseDateTimeField_MockStandardDateTimeField0.getRangeDurationField();
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.get(63072000000L);
      testPreciseDateTimeField_MockStandardDateTimeField0.getRangeDurationField();
      testPreciseDateTimeField_MockStandardDateTimeField0.get(60000L);
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.getMinimumValue();
      testPreciseDateTimeField_MockStandardDateTimeField0.getMaximumValueForSet(41000L, 0);
      // Undeclared exception!
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.set(0L, 604800);
  }
}
