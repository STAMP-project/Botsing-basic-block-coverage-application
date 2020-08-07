/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 23:41:26 UTC 2020
 */

package org.joda.time.chrono;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DurationFieldType;
import org.joda.time.Hours;
import org.joda.time.LocalDateTime;
import org.joda.time.LocalTime;
import org.joda.time.ReadablePeriod;
import org.joda.time.chrono.CopticChronology;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseChronology_ESTest extends BaseChronology_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Hours hours0 = Hours.EIGHT;
      Hours hours1 = Hours.FIVE;
      CopticChronology copticChronology0 = CopticChronology.getInstance();
      LocalTime localTime0 = new LocalTime();
      Hours hours2 = Hours.FIVE;
      CopticChronology copticChronology1 = CopticChronology.getInstance();
      DurationFieldType durationFieldType0 = DurationFieldType.eras();
      hours2.isSupported(durationFieldType0);
      localTime0.minusSeconds((-36));
      copticChronology1.set(localTime0, (-36));
      int int0 = 0;
      copticChronology0.getDateTimeMillis((long) 0, 4, 4, 0, 89);
      CopticChronology copticChronology2 = CopticChronology.getInstanceUTC();
      copticChronology1.weekyears();
      LocalDateTime localDateTime0 = new LocalDateTime((long) (-374));
      localDateTime0.plus((ReadablePeriod) hours1);
      // Undeclared exception!
      copticChronology2.set(localDateTime0, (-36L));
  }
}
