/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat Jan 18 06:24:13 UTC 2020
 */

package org.joda.time;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class Period_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.joda.time.Period"; 
    org.evosuite.runtime.GuiSupport.initialize(); 
    org.evosuite.runtime.RuntimeSettings.maxNumberOfIterationsPerLoop = 10000; 
    org.evosuite.runtime.RuntimeSettings.mockSystemIn = true; 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
  } 

  @Before 
  public void initTestCase(){ 
    threadStopper.storeCurrentThreads();
    threadStopper.startRecordingTime();
    org.evosuite.runtime.GuiSupport.setHeadless(); 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    org.evosuite.runtime.agent.InstrumentingAgent.activate(); 
  } 

  @After 
  public void doneWithTestCase(){ 
    threadStopper.killAndJoinClientThreads();
    org.evosuite.runtime.agent.InstrumentingAgent.deactivate(); 
    org.evosuite.runtime.GuiSupport.restoreHeadlessMode(); 
  } 


  private static void initializeClasses() {
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(Period_ESTest_scaffolding.class.getClassLoader() ,
      "org.joda.time.DateTimeZone",
      "org.joda.time.tz.DateTimeZoneBuilder$Recurrence",
      "org.joda.time.convert.ReadablePartialConverter",
      "org.joda.time.DateTimeUtils$MillisProvider",
      "org.joda.time.convert.Converter",
      "org.joda.time.chrono.GJYearOfEraDateTimeField",
      "org.joda.time.convert.PartialConverter",
      "org.joda.time.Seconds",
      "org.joda.time.field.RemainderDateTimeField",
      "org.joda.time.JodaTimePermission",
      "org.joda.time.chrono.BasicWeekOfWeekyearDateTimeField",
      "org.joda.time.DateTimeFieldType",
      "org.joda.time.convert.ConverterSet$Entry",
      "org.joda.time.DateTimeFieldType$StandardDateTimeFieldType",
      "org.joda.time.ReadableInterval",
      "org.joda.time.chrono.BasicChronology$HalfdayField",
      "org.joda.time.tz.DateTimeZoneBuilder$PrecalculatedZone",
      "org.joda.time.chrono.BasicChronology$YearInfo",
      "org.joda.time.field.UnsupportedDurationField",
      "org.joda.time.convert.AbstractConverter",
      "org.joda.time.format.PeriodFormatterBuilder",
      "org.joda.time.chrono.ISOChronology",
      "org.joda.time.chrono.BasicChronology",
      "org.joda.time.chrono.BasicYearDateTimeField",
      "org.joda.time.format.PeriodFormatterBuilder$FieldFormatter",
      "org.joda.time.field.DividedDateTimeField",
      "org.joda.time.convert.DateConverter",
      "org.joda.time.chrono.ZonedChronology",
      "org.joda.time.field.BaseDateTimeField",
      "org.joda.time.field.ZeroIsMaxDateTimeField",
      "org.joda.time.tz.CachedDateTimeZone$Info",
      "org.joda.time.base.BaseInterval",
      "org.joda.time.Duration",
      "org.joda.time.PeriodType",
      "org.joda.time.format.PeriodFormatter",
      "org.joda.time.field.MillisDurationField",
      "org.joda.time.chrono.GJChronology",
      "org.joda.time.Interval",
      "org.joda.time.convert.LongConverter",
      "org.joda.time.tz.DateTimeZoneBuilder",
      "org.joda.time.field.UnsupportedDateTimeField",
      "org.joda.time.field.ScaledDurationField",
      "org.joda.time.chrono.ISOYearOfEraDateTimeField",
      "org.joda.time.ReadWritablePeriod",
      "org.joda.time.convert.ConverterSet",
      "org.joda.time.field.PreciseDurationDateTimeField",
      "org.joda.time.MutablePeriod",
      "org.joda.time.base.BasePeriod$1",
      "org.joda.time.tz.FixedDateTimeZone",
      "org.joda.time.format.PeriodPrinter",
      "org.joda.time.convert.IntervalConverter",
      "org.joda.time.tz.CachedDateTimeZone",
      "org.joda.time.field.PreciseDateTimeField",
      "org.joda.time.format.PeriodFormatterBuilder$Literal",
      "org.joda.time.format.PeriodParser",
      "org.joda.time.convert.ReadableDurationConverter",
      "org.joda.time.base.BaseDuration",
      "org.joda.time.field.DecoratedDateTimeField",
      "org.joda.time.convert.DurationConverter",
      "org.joda.time.field.OffsetDateTimeField",
      "org.joda.time.convert.ReadablePeriodConverter",
      "org.joda.time.chrono.GJMonthOfYearDateTimeField",
      "org.joda.time.convert.ReadableIntervalConverter",
      "org.joda.time.chrono.BasicWeekyearDateTimeField",
      "org.joda.time.Days",
      "org.joda.time.DateTimeField",
      "org.joda.time.field.FieldUtils",
      "org.joda.time.base.AbstractPeriod",
      "org.joda.time.ReadableInstant",
      "org.joda.time.DateTimeUtils$SystemMillisProvider",
      "org.joda.time.chrono.GJDayOfWeekDateTimeField",
      "org.joda.time.base.BaseSingleFieldPeriod",
      "org.joda.time.IllegalInstantException",
      "org.joda.time.IllegalFieldValueException",
      "org.joda.time.convert.NullConverter",
      "org.joda.time.tz.DefaultNameProvider",
      "org.joda.time.tz.Provider",
      "org.joda.time.field.ImpreciseDateTimeField$LinkedDurationField",
      "org.joda.time.ReadablePeriod",
      "org.joda.time.chrono.ZonedChronology$ZonedDateTimeField",
      "org.joda.time.chrono.GregorianChronology",
      "org.joda.time.chrono.AssembledChronology$Fields",
      "org.joda.time.DurationFieldType",
      "org.joda.time.convert.ConverterManager",
      "org.joda.time.tz.NameProvider",
      "org.joda.time.Minutes",
      "org.joda.time.chrono.BasicMonthOfYearDateTimeField",
      "org.joda.time.DateTimeUtils",
      "org.joda.time.base.AbstractDuration",
      "org.joda.time.Hours",
      "org.joda.time.base.AbstractInterval",
      "org.joda.time.convert.ReadableInstantConverter",
      "org.joda.time.convert.StringConverter",
      "org.joda.time.base.BasePeriod",
      "org.joda.time.field.DecoratedDurationField",
      "org.joda.time.tz.DateTimeZoneBuilder$DSTZone",
      "org.joda.time.convert.InstantConverter",
      "org.joda.time.chrono.AssembledChronology",
      "org.joda.time.format.ISOPeriodFormat",
      "org.joda.time.tz.ZoneInfoProvider",
      "org.joda.time.chrono.GJEraDateTimeField",
      "org.joda.time.DateTimeZone$1",
      "org.joda.time.chrono.BaseChronology",
      "org.joda.time.chrono.JulianChronology",
      "org.joda.time.field.ImpreciseDateTimeField",
      "org.joda.time.field.PreciseDurationField",
      "org.joda.time.Period",
      "org.joda.time.tz.DateTimeZoneBuilder$OfYear",
      "org.joda.time.ReadableDuration",
      "org.joda.time.chrono.BasicGJChronology",
      "org.joda.time.DurationField",
      "org.joda.time.Weeks",
      "org.joda.time.Chronology",
      "org.joda.time.format.PeriodFormatterBuilder$Composite",
      "org.joda.time.format.PeriodFormatterBuilder$PeriodFieldAffix",
      "org.joda.time.format.PeriodFormatterBuilder$SimpleAffix",
      "org.joda.time.chrono.BasicDayOfMonthDateTimeField",
      "org.joda.time.convert.PeriodConverter",
      "org.joda.time.chrono.ZonedChronology$ZonedDurationField",
      "org.joda.time.format.PeriodFormatterBuilder$Separator",
      "org.joda.time.convert.CalendarConverter",
      "org.joda.time.ReadablePartial",
      "org.joda.time.DurationFieldType$StandardDurationFieldType",
      "org.joda.time.chrono.BasicDayOfYearDateTimeField",
      "org.joda.time.field.BaseDurationField",
      "org.joda.time.chrono.BuddhistChronology"
    );
  } 
}
