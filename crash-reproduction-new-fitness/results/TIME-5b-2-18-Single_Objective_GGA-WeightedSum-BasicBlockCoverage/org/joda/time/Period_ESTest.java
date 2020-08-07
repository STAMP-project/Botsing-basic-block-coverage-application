/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 16:44:55 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Hours;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.ReadableInstant;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ReadableInstant readableInstant0 = null;
      Hours hours0 = Hours.SEVEN;
      PeriodType periodType0 = hours0.getPeriodType();
      PeriodType periodType1 = periodType0.withDaysRemoved();
      Period period0 = new Period((ReadableInstant) null, (ReadableInstant) null, periodType1);
      Period period1 = period0.minus(hours0);
      int int0 = (-972);
      // Undeclared exception!
      period1.withYears((-972));
  }
}
