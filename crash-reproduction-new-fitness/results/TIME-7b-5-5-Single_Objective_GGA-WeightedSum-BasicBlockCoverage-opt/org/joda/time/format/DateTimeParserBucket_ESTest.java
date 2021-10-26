/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:36:11 UTC 2021
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
import org.joda.time.Chronology;
import org.joda.time.DateTimeField;
import org.joda.time.DateTimeZone;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.chrono.CopticChronology;
import org.joda.time.chrono.LenientChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Chronology chronology0 = mock(Chronology.class, new ViolatedAssumptionAnswer());
      doReturn((DateTimeZone) null).when(chronology0).getZone();
      doReturn((Chronology) null).when(chronology0).withUTC();
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket((-667L), chronology0, (Locale) null);
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstance();
      DateTimeField dateTimeField0 = buddhistChronology0.dayOfWeek();
      String string0 = "Cannot have two adjacent separators";
      buddhistChronology0.equals("Cannot have two adjacent separators");
      dateTimeParserBucket0.saveField(dateTimeField0, 391);
      int int0 = 0;
      dateTimeParserBucket0.saveState();
      CopticChronology copticChronology0 = CopticChronology.getInstance();
      copticChronology0.add(0L, (long) 0, 0);
      LenientChronology.getInstance(copticChronology0);
      Integer integer0 = new Integer(0);
      String string1 = "@g}ZA1";
      Integer integer1 = new Integer(0);
      Integer.toUnsignedString(0);
      dateTimeParserBucket0.setPivotYear(integer0);
      dateTimeParserBucket0.getOffsetInteger();
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(false);
  }
}
