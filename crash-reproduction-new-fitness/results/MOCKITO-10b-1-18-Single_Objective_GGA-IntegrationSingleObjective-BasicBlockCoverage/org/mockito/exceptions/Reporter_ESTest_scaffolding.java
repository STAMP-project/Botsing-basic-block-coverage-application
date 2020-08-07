/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 03:32:45 UTC 2020
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
      "org.mockito.configuration.AnnotationEngine",
      "org.mockito.cglib.proxy.Callback",
      "org.mockito.exceptions.misusing.UnfinishedVerificationException",
      "org.mockito.invocation.Invocation",
      "org.mockito.invocation.Location",
      "org.mockito.internal.invocation.InvocationMatcher",
      "org.mockito.internal.stubbing.StubbedInvocationMatcher",
      "org.mockito.exceptions.Reporter",
      "org.mockito.exceptions.verification.VerificationInOrderFailure",
      "org.mockito.internal.exceptions.VerificationAwareInvocation",
      "org.mockito.configuration.DefaultMockitoConfiguration",
      "org.mockito.cglib.reflect.FastClass",
      "org.mockito.exceptions.misusing.NullInsteadOfMockException",
      "org.mockito.internal.invocation.realmethod.FilteredCGLIBProxyRealMethod",
      "org.mockito.internal.creation.DelegatingMockitoMethodProxy",
      "org.mockito.internal.util.StringJoiner",
      "org.mockito.internal.exceptions.stacktrace.StackTraceFilter",
      "org.mockito.exceptions.misusing.NotAMockException",
      "org.mockito.internal.invocation.realmethod.RealMethod",
      "org.mockito.exceptions.misusing.FriendlyReminderException",
      "org.mockito.exceptions.misusing.MissingMethodInvocationException",
      "org.mockito.exceptions.verification.SmartNullPointerException",
      "org.mockito.exceptions.verification.TooLittleActualInvocations",
      "org.mockito.invocation.StubInfo",
      "org.mockito.internal.configuration.GlobalConfiguration",
      "org.mockito.internal.invocation.AbstractAwareMethod",
      "org.mockito.internal.creation.MockitoMethodProxy",
      "org.mockito.cglib.proxy.MethodInterceptor",
      "org.mockito.exceptions.verification.TooManyActualInvocations",
      "org.mockito.plugins.MockMaker",
      "org.mockito.exceptions.verification.ArgumentsAreDifferent",
      "org.mockito.internal.invocation.InvocationImpl",
      "org.mockito.internal.exceptions.stacktrace.DefaultStackTraceCleanerProvider",
      "org.mockito.plugins.StackTraceCleanerProvider",
      "org.mockito.exceptions.misusing.MockitoConfigurationException",
      "org.mockito.internal.invocation.MockitoMethod",
      "org.mockito.internal.exceptions.stacktrace.ConditionalStackTraceFilter",
      "org.mockito.cglib.proxy.MethodProxy$CreateInfo",
      "org.mockito.configuration.MockitoConfiguration",
      "org.mockito.exceptions.misusing.InvalidUseOfMatchersException",
      "org.mockito.exceptions.misusing.UnfinishedStubbingException",
      "org.mockito.exceptions.verification.NoInteractionsWanted",
      "org.mockito.internal.creation.AbstractMockitoMethodProxy",
      "org.mockito.internal.invocation.StubInfoImpl",
      "org.mockito.internal.invocation.CapturesArgumensFromInvocation",
      "org.mockito.exceptions.PrintableInvocation",
      "org.mockito.listeners.InvocationListener",
      "org.mockito.internal.invocation.SerializableMethod",
      "org.mockito.cglib.proxy.MethodProxy$FastClassInfo",
      "org.mockito.invocation.DescribedInvocation",
      "org.mockito.internal.configuration.ClassPathLoader",
      "org.mockito.exceptions.base.MockitoException",
      "org.mockito.internal.invocation.realmethod.HasCGLIBMethodProxy",
      "org.mockito.asm.Type",
      "org.mockito.stubbing.Answer",
      "org.mockito.invocation.InvocationOnMock",
      "org.mockito.exceptions.verification.WantedButNotInvoked",
      "org.mockito.Incubating",
      "org.mockito.internal.creation.CglibMockMaker",
      "org.mockito.stubbing.answers.ReturnsElementsOf",
      "org.mockito.cglib.core.Signature",
      "org.mockito.configuration.IMockitoConfiguration",
      "org.mockito.internal.stubbing.answers.ReturnsElementsOf",
      "org.mockito.exceptions.misusing.WrongTypeOfReturnValue",
      "org.mockito.internal.exceptions.stacktrace.DefaultStackTraceCleaner",
      "org.mockito.exceptions.stacktrace.StackTraceCleaner",
      "org.mockito.exceptions.base.MockitoAssertionError",
      "org.mockito.internal.reporting.Discrepancy",
      "org.mockito.internal.debugging.LocationImpl",
      "org.mockito.cglib.proxy.MethodProxy",
      "org.mockito.exceptions.verification.NeverWantedButInvoked",
      "org.mockito.cglib.core.CodeGenerationException",
      "org.mockito.exceptions.misusing.CannotStubVoidMethodWithReturnValue",
      "org.mockito.exceptions.misusing.CannotVerifyStubOnlyMock",
      "org.mockito.internal.creation.SerializableMockitoMethodProxy",
      "org.mockito.internal.invocation.realmethod.CGLIBProxyRealMethod"
    );
  } 
}
