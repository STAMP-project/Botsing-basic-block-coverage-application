/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:20:32 UTC 2020
 */

package org.joda.time.chrono;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.MonthDay;
import org.joda.time.chrono.EthiopicChronology;
import org.joda.time.chrono.GregorianChronology;
import org.joda.time.chrono.JulianChronology;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseChronology_ESTest extends BaseChronology_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      GregorianChronology gregorianChronology0 = GregorianChronology.getInstance();
      EthiopicChronology ethiopicChronology0 = EthiopicChronology.getInstance();
      gregorianChronology0.era();
      gregorianChronology0.clockhourOfDay();
      ethiopicChronology0.era();
      JulianChronology julianChronology0 = JulianChronology.getInstanceUTC();
      MonthDay monthDay0 = new MonthDay((-894L));
      MonthDay monthDay1 = new MonthDay((long) 2375);
      monthDay1.minusDays(32);
      julianChronology0.set(monthDay1, 32);
      // Undeclared exception!
      ethiopicChronology0.set(monthDay0, 32);
  }
}
