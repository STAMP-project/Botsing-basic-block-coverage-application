/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 05:08:24 UTC 2020
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
import org.joda.time.DateTime;
import org.joda.time.ReadableInstant;
import org.joda.time.chrono.ISOChronology;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.DateTimeParser;
import org.joda.time.format.DateTimePrinter;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeFormatter_ESTest extends DateTimeFormatter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimePrinter dateTimePrinter0 = mock(DateTimePrinter.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null).when(dateTimePrinter0).toString();
      doReturn(0).when(dateTimePrinter0).estimatePrintedLength();
      DateTimeParser dateTimeParser0 = mock(DateTimeParser.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null).when(dateTimeParser0).toString();
      doReturn(0, 0).when(dateTimeParser0).parseInto(any(org.joda.time.format.DateTimeParserBucket.class) , anyString() , anyInt());
      DateTimeFormatter dateTimeFormatter0 = new DateTimeFormatter(dateTimePrinter0, dateTimeParser0);
      Locale locale0 = Locale.CANADA_FRENCH;
      DateTimeFormatter dateTimeFormatter1 = dateTimeFormatter0.withLocale(locale0);
      ISOChronology iSOChronology0 = ISOChronology.getInstance();
      DateTime dateTime0 = DateTime.now((Chronology) iSOChronology0);
      dateTimeFormatter0.print((ReadableInstant) dateTime0);
      dateTimeFormatter1.parseDateTime("");
      dateTimeFormatter0.getDefaultYear();
      dateTimeFormatter1.isOffsetParsed();
      dateTimeFormatter0.getPivotYear();
      // Undeclared exception!
      dateTimeFormatter1.parseDateTime("+00");
  }
}
