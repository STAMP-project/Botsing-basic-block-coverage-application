/*
 * This file was automatically generated by EvoSuite
 * Fri May 15 12:22:49 UTC 2020
 */

package org.joda.time.format;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.joda.time.DateTimeField;
import org.joda.time.MockPartial;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.chrono.IslamicChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket$SavedField_ESTest extends DateTimeParserBucket$SavedField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeField dateTimeField0 = mock(DateTimeField.class, new ViolatedAssumptionAnswer());
      doReturn(0L).when(dateTimeField0).roundFloor(anyLong());
      doReturn(0L, 0L).when(dateTimeField0).set(anyLong() , anyString() , any(java.util.Locale.class));
      String string0 = "";
      Locale locale0 = Locale.TRADITIONAL_CHINESE;
      DateTimeParserBucket.SavedField dateTimeParserBucket_SavedField0 = new DateTimeParserBucket.SavedField(dateTimeField0, "", locale0);
      IslamicChronology islamicChronology0 = IslamicChronology.getInstanceUTC();
      islamicChronology0.withUTC();
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstance();
      dateTimeParserBucket_SavedField0.set((-3839L), true);
      buddhistChronology0.year();
      MockPartial mockPartial0 = new MockPartial();
      DateTimeField dateTimeField1 = buddhistChronology0.dayOfWeek();
      DateTimeParserBucket.SavedField dateTimeParserBucket_SavedField1 = new DateTimeParserBucket.SavedField(dateTimeField1, 580);
      dateTimeParserBucket_SavedField0.set(580, false);
      // Undeclared exception!
      dateTimeParserBucket_SavedField1.set(42474480000L, false);
  }
}
