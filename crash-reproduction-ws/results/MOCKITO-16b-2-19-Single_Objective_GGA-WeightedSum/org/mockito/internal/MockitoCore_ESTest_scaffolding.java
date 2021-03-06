/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 12:28:01 UTC 2020
 */

package org.mockito.internal;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class MockitoCore_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.mockito.internal.MockitoCore"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(MockitoCore_ESTest_scaffolding.class.getClassLoader() ,
      "org.mockito.cglib.proxy.Callback",
      "org.mockito.configuration.AnnotationEngine",
      "org.mockito.exceptions.misusing.UnfinishedVerificationException",
      "org.mockito.exceptions.Reporter",
      "org.mockito.exceptions.verification.VerificationInOrderFailure",
      "org.mockito.configuration.DefaultMockitoConfiguration",
      "org.mockito.exceptions.misusing.NullInsteadOfMockException",
      "org.mockito.stubbing.VoidMethodStubbable",
      "org.mockito.internal.util.StringJoiner",
      "org.mockito.exceptions.misusing.NotAMockException",
      "org.mockito.internal.progress.MockingProgress",
      "org.mockito.internal.util.MockitoLogger",
      "org.mockito.exceptions.misusing.MissingMethodInvocationException",
      "org.mockito.exceptions.verification.SmartNullPointerException",
      "org.mockito.exceptions.verification.TooLittleActualInvocations",
      "org.mockito.internal.progress.MockingProgressImpl",
      "org.mockito.internal.configuration.GlobalConfiguration",
      "org.mockito.stubbing.DeprecatedOngoingStubbing",
      "org.mockito.cglib.proxy.MethodInterceptor",
      "org.mockito.internal.reporting.PrintingFriendlyInvocation",
      "org.mockito.exceptions.verification.TooManyActualInvocations",
      "org.mockito.exceptions.verification.ArgumentsAreDifferent",
      "org.mockito.exceptions.misusing.MockitoConfigurationException",
      "org.mockito.internal.progress.IOngoingStubbing",
      "org.hamcrest.SelfDescribing",
      "org.mockito.configuration.MockitoConfiguration",
      "org.mockito.exceptions.misusing.InvalidUseOfMatchersException",
      "org.mockito.internal.invocation.Invocation",
      "org.mockito.exceptions.misusing.UnfinishedStubbingException",
      "org.mockito.internal.debugging.DebuggingInfo",
      "org.mockito.exceptions.verification.NoInteractionsWanted",
      "org.mockito.internal.MockitoCore",
      "org.mockito.exceptions.PrintableInvocation",
      "org.mockito.internal.configuration.ClassPathLoader",
      "org.mockito.exceptions.base.MockitoException",
      "org.mockito.stubbing.Stubber",
      "org.mockito.internal.exceptions.base.ConditionalStackTraceFilter",
      "org.mockito.stubbing.Answer",
      "org.mockito.internal.exceptions.base.StackTraceFilter",
      "org.mockito.invocation.InvocationOnMock",
      "org.mockito.internal.progress.ArgumentMatcherStorageImpl",
      "org.mockito.exceptions.verification.WantedButNotInvoked",
      "org.mockito.internal.progress.ThreadSafeMockingProgress",
      "org.mockito.internal.verification.api.VerificationMode",
      "org.mockito.configuration.IMockitoConfiguration",
      "org.mockito.InOrder",
      "org.mockito.internal.MockitoInvocationHandler",
      "org.mockito.exceptions.misusing.WrongTypeOfReturnValue",
      "org.mockito.MockSettings",
      "org.mockito.stubbing.OngoingStubbing",
      "org.mockito.internal.progress.ArgumentMatcherStorage",
      "org.mockito.exceptions.base.MockitoAssertionError",
      "org.mockito.internal.util.MockUtil",
      "org.mockito.exceptions.verification.NeverWantedButInvoked",
      "org.mockito.internal.util.CreationValidator",
      "org.hamcrest.Matcher"
    );
  } 
}
