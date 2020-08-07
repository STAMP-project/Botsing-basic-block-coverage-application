/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 17:42:17 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Duration;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.Weeks;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Period period0 = Period.years(2211);
      Period period1 = Period.hours(2211);
      period1.getHours();
      Period.days(2211);
      period0.getDays();
      Period period2 = period0.withHours(13);
      period2.negated();
      Weeks weeks0 = Weeks.parseWeeks((String) null);
      Duration duration0 = weeks0.toStandardDuration();
      PeriodType periodType0 = PeriodType.hours();
      Period period3 = duration0.toPeriod(periodType0);
      // Undeclared exception!
      period3.withYears(0);
  }
}
