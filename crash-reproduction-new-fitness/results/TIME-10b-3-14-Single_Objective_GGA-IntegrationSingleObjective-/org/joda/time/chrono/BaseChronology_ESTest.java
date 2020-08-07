/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:21:24 UTC 2020
 */

package org.joda.time.chrono;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeZone;
import org.joda.time.LocalDate;
import org.joda.time.chrono.CopticChronology;
import org.joda.time.chrono.JulianChronology;
import org.joda.time.tz.UTCProvider;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseChronology_ESTest extends BaseChronology_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      JulianChronology julianChronology0 = JulianChronology.getInstance();
      julianChronology0.minuteOfHour();
      julianChronology0.secondOfMinute();
      int int0 = 0;
      UTCProvider uTCProvider0 = new UTCProvider();
      uTCProvider0.getZone("Yr(rC<H<k(w(x{mC");
      CopticChronology copticChronology0 = CopticChronology.getInstance((DateTimeZone) null);
      copticChronology0.yearOfCentury();
      DateTimeZone dateTimeZone0 = julianChronology0.getZone();
      copticChronology0.withZone((DateTimeZone) null);
      LocalDate localDate0 = new LocalDate((-1449L), dateTimeZone0);
      // Undeclared exception!
      copticChronology0.set(localDate0, 448L);
  }
}
