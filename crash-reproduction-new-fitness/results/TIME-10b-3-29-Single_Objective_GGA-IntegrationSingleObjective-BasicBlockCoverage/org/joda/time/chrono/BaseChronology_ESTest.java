/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 04:45:46 UTC 2020
 */

package org.joda.time.chrono;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Chronology;
import org.joda.time.DateMidnight;
import org.joda.time.LocalDate;
import org.joda.time.chrono.CopticChronology;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseChronology_ESTest extends BaseChronology_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      CopticChronology copticChronology0 = CopticChronology.getInstanceUTC();
      copticChronology0.centuryOfEra();
      copticChronology0.minuteOfHour();
      copticChronology0.millisOfDay();
      copticChronology0.clockhourOfDay();
      copticChronology0.dayOfWeek();
      copticChronology0.yearOfEra();
      copticChronology0.secondOfMinute();
      DateMidnight dateMidnight0 = new DateMidnight(1589777115389L, (Chronology) copticChronology0);
      copticChronology0.dayOfWeek();
      copticChronology0.yearOfEra();
      int int0 = (-1532);
      DateMidnight dateMidnight1 = new DateMidnight((-2976L), (Chronology) null);
      LocalDate localDate0 = dateMidnight1.toLocalDate();
      localDate0.plusYears((-1532));
      // Undeclared exception!
      copticChronology0.set(localDate0, 0L);
  }
}
