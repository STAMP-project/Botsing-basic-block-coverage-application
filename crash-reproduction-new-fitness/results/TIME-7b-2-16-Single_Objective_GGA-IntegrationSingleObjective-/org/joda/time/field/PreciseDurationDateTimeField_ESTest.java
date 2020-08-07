/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 18:29:42 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.field.MillisDurationField;
import org.joda.time.field.TestPreciseDateTimeField;
import org.joda.time.field.TestPreciseDurationDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PreciseDurationDateTimeField_ESTest extends PreciseDurationDateTimeField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MillisDurationField millisDurationField0 = (MillisDurationField)MillisDurationField.INSTANCE;
      TestPreciseDurationDateTimeField.MockStandardBaseDateTimeField testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0 = new TestPreciseDurationDateTimeField.MockStandardBaseDateTimeField();
      int[] intArray0 = new int[5];
      intArray0[0] = (-598);
      intArray0[2] = (-598);
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.getMaximumValue((-891L));
      intArray0[3] = 0;
      intArray0[4] = (-598);
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.roundHalfFloor(86400000L);
      Locale locale0 = new Locale(" p=i;vm8DdKL#pDwG", " p=i;vm8DdKL#pDwG", "GMT+");
      Locale.Category locale_Category0 = Locale.Category.FORMAT;
      Locale.setDefault(locale_Category0, locale0);
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.getAsShortText((long) (-598), locale0);
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.isLenient();
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.getRangeDurationField();
      TestPreciseDateTimeField.MockStandardDateTimeField testPreciseDateTimeField_MockStandardDateTimeField0 = new TestPreciseDateTimeField.MockStandardDateTimeField();
      int[] intArray1 = new int[3];
      intArray1[0] = (-292269337);
      intArray1[1] = (-24);
      testPreciseDateTimeField_MockStandardDateTimeField0.addWrapField((-1L), 0);
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.roundCeiling((-9223372036854775808L));
      // Undeclared exception!
      testPreciseDurationDateTimeField_MockStandardBaseDateTimeField0.set(255L, (-292269337));
  }
}
