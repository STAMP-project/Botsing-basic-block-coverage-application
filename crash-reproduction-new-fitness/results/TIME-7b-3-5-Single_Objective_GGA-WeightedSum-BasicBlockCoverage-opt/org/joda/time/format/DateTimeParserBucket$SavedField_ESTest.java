/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:36:09 UTC 2021
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
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket$SavedField_ESTest extends DateTimeParserBucket$SavedField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeField dateTimeField0 = mock(DateTimeField.class, new ViolatedAssumptionAnswer());
      doReturn(0L, 0L, 0L).when(dateTimeField0).set(anyLong() , anyString() , any(java.util.Locale.class));
      Locale locale0 = Locale.UK;
      DateTimeParserBucket.SavedField dateTimeParserBucket_SavedField0 = new DateTimeParserBucket.SavedField(dateTimeField0, "#B_p=ZYK4t`BLYV/!", locale0);
      dateTimeParserBucket_SavedField0.set(1644L, false);
      dateTimeParserBucket_SavedField0.set(0L, false);
      dateTimeParserBucket_SavedField0.set(0L, false);
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstance();
      DateTimeField dateTimeField1 = buddhistChronology0.dayOfWeek();
      DateTimeParserBucket.SavedField dateTimeParserBucket_SavedField1 = new DateTimeParserBucket.SavedField(dateTimeField1, 48);
      // Undeclared exception!
      dateTimeParserBucket_SavedField1.set(0L, true);
  }
}
