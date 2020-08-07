/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sun May 17 00:13:52 UTC 2020
 */

package org.mockito.exceptions;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class Reporter_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.mockito.exceptions.Reporter"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(Reporter_ESTest_scaffolding.class.getClassLoader() ,
      "org.mockito.internal.matchers.ContainsExtraTypeInformation",
      "org.mockito.internal.handler.InvocationNotifierHandlerTest$CustomListener",
      "org.mockito.cglib.proxy.Callback",
      "org.mockito.exceptions.misusing.UnfinishedVerificationException",
      "org.hamcrest.StringDescription",
      "org.mockito.invocation.Location",
      "org.mockito.mock.SerializableMode",
      "org.mockito.internal.invocation.InvocationMatcher",
      "org.hamcrest.core.Is",
      "org.mockito.internal.debugging.VerboseMockInvocationLogger",
      "org.mockito.internal.exceptions.stacktrace.StackTraceFilter",
      "org.mockito.exceptions.misusing.NotAMockException",
      "org.mockito.internal.invocation.realmethod.RealMethod",
      "org.mockito.exceptions.misusing.FriendlyReminderException",
      "org.mockito.exceptions.misusing.MissingMethodInvocationException",
      "org.mockito.exceptions.verification.SmartNullPointerException",
      "org.mockito.exceptions.verification.TooLittleActualInvocations",
      "org.hamcrest.core.DescribedAs",
      "org.mockito.internal.configuration.GlobalConfiguration",
      "org.mockito.cglib.proxy.MethodInterceptor",
      "org.mockito.exceptions.verification.TooManyActualInvocations",
      "org.mockito.plugins.MockMaker",
      "org.mockito.internal.invocation.InvocationImpl",
      "org.mockito.exceptions.misusing.MockitoConfigurationException",
      "org.mockito.internal.invocation.MockitoMethod",
      "org.mockito.internal.exceptions.stacktrace.ConditionalStackTraceFilter",
      "org.hamcrest.BaseDescription",
      "org.hamcrest.core.IsNot",
      "org.mockito.listeners.InvocationListener",
      "org.mockito.internal.invocation.SerializableMethod",
      "org.hamcrest.core.AllOf",
      "org.mockito.internal.creation.cglib.SerializableMockitoMethodProxy",
      "org.mockito.stubbing.Answer",
      "org.hamcrest.BaseMatcher",
      "org.mockito.invocation.InvocationOnMock",
      "org.mockito.Incubating",
      "org.hamcrest.core.IsEqual",
      "org.mockito.internal.creation.util.MockitoMethodProxy",
      "org.mockito.internal.creation.cglib.CglibMockMaker",
      "org.hamcrest.core.IsSame",
      "org.mockito.configuration.IMockitoConfiguration",
      "org.mockito.exceptions.misusing.WrongTypeOfReturnValue",
      "org.mockito.internal.exceptions.stacktrace.DefaultStackTraceCleaner",
      "org.mockito.exceptions.base.MockitoAssertionError",
      "org.mockito.internal.debugging.LocationImpl",
      "org.mockito.internal.matchers.LocalizedMatcher",
      "org.mockito.exceptions.verification.NeverWantedButInvoked",
      "org.mockito.exceptions.misusing.CannotVerifyStubOnlyMock",
      "org.hamcrest.Matcher",
      "org.hamcrest.core.IsNull",
      "org.mockito.configuration.AnnotationEngine",
      "org.mockito.invocation.Invocation",
      "org.mockito.listeners.MethodInvocationReport",
      "org.mockito.internal.stubbing.StubbedInvocationMatcher",
      "org.mockito.exceptions.Reporter",
      "org.mockito.exceptions.verification.VerificationInOrderFailure",
      "org.mockito.internal.exceptions.VerificationAwareInvocation",
      "org.mockito.configuration.DefaultMockitoConfiguration",
      "org.mockito.internal.listeners.NotifiedMethodInvocationReport",
      "org.mockito.exceptions.misusing.NullInsteadOfMockException",
      "org.mockito.internal.util.StringJoiner",
      "org.mockito.invocation.StubInfo",
      "org.mockito.internal.matchers.MatcherDecorator",
      "org.mockito.internal.invocation.AbstractAwareMethod",
      "org.mockito.exceptions.verification.ArgumentsAreDifferent",
      "org.mockito.internal.exceptions.stacktrace.DefaultStackTraceCleanerProvider",
      "org.mockito.plugins.StackTraceCleanerProvider",
      "org.hamcrest.SelfDescribing",
      "org.mockito.configuration.MockitoConfiguration",
      "org.mockito.exceptions.misusing.InvalidUseOfMatchersException",
      "org.mockito.exceptions.misusing.UnfinishedStubbingException",
      "org.mockito.internal.invocation.realmethod.DefaultRealMethod",
      "org.mockito.exceptions.verification.junit.ArgumentsAreDifferent",
      "org.mockito.exceptions.verification.NoInteractionsWanted",
      "org.hamcrest.internal.SelfDescribingValue",
      "org.mockito.internal.invocation.StubInfoImpl",
      "org.mockito.internal.invocation.CapturesArgumensFromInvocation",
      "org.mockito.exceptions.PrintableInvocation",
      "org.mockito.invocation.DescribedInvocation",
      "org.mockito.internal.configuration.ClassPathLoader",
      "org.mockito.exceptions.base.MockitoException",
      "org.mockito.internal.matchers.CapturesArguments",
      "org.hamcrest.core.AnyOf",
      "org.mockito.exceptions.verification.WantedButNotInvoked",
      "org.hamcrest.core.IsAnything",
      "org.hamcrest.Factory",
      "org.mockito.stubbing.answers.ReturnsElementsOf",
      "org.hamcrest.core.IsInstanceOf",
      "org.mockito.internal.invocation.realmethod.CleanTraceRealMethod",
      "org.hamcrest.Description",
      "org.mockito.internal.stubbing.answers.ReturnsElementsOf",
      "org.mockito.internal.junit.JUnitTool",
      "org.mockito.internal.creation.cglib.DelegatingMockitoMethodProxy",
      "org.mockito.internal.reporting.Pluralizer",
      "org.mockito.exceptions.stacktrace.StackTraceCleaner",
      "org.mockito.internal.reporting.Discrepancy",
      "org.mockito.exceptions.misusing.CannotStubVoidMethodWithReturnValue"
    );
  } 
}
