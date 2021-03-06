/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 13:24:27 UTC 2020
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
import org.joda.time.DateTimeFieldType;
import org.joda.time.MutableDateTime;
import org.joda.time.chrono.JulianChronology;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.DateTimeFormatterBuilder;
import org.joda.time.format.DateTimeParser;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeFormatter_ESTest extends DateTimeFormatter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeParser dateTimeParser0 = mock(DateTimeParser.class, new ViolatedAssumptionAnswer());
      MutableDateTime mutableDateTime0 = MutableDateTime.now();
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.dayOfMonth();
      dateTimeFieldType0.getRangeDurationType();
      DateTimeFormatterBuilder.TwoDigitYear dateTimeFormatterBuilder_TwoDigitYear0 = new DateTimeFormatterBuilder.TwoDigitYear(dateTimeFieldType0, (-1901), true);
      DateTimeFormatter dateTimeFormatter0 = new DateTimeFormatter(dateTimeFormatterBuilder_TwoDigitYear0, dateTimeFormatterBuilder_TwoDigitYear0);
      MutableDateTime mutableDateTime1 = new MutableDateTime();
      DateTimeFormatter dateTimeFormatter1 = dateTimeFormatter0.withOffsetParsed();
      dateTimeFormatter0.parseInto(mutableDateTime0, "9O</E+m<aa4`}42", 10);
      DateTimeFormatter dateTimeFormatter2 = dateTimeFormatter1.withLocale((Locale) null);
      JulianChronology.getInstanceUTC();
      // Undeclared exception!
      dateTimeFormatter2.parseInto(mutableDateTime0, "9223372036854775808", 10);
  }
}
