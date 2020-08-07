/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 03:53:09 UTC 2020
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
      "org.mockito.exceptions.misusing.UnfinishedVerificationException",
      "org.mockito.internal.stubbing.InvocationContainer",
      "org.mockito.internal.invocation.InvocationMatcher",
      "org.mockito.internal.creation.MockSettingsImpl",
      "org.mockito.internal.verification.AtLeast",
      "org.mockitousage.basicapi.AnnotationsTest$Base",
      "org.mockito.internal.creation.jmock.ClassImposterizer$3",
      "org.mockito.internal.stubbing.InvocationContainerImpl",
      "org.mockito.internal.invocation.realmethod.FilteredCGLIBProxyRealMethod",
      "org.mockito.exceptions.misusing.NotAMockException",
      "org.mockito.internal.stubbing.OngoingStubbingImpl",
      "org.mockito.internal.invocation.realmethod.RealMethod",
      "org.mockito.MockitoAnnotations$Mock",
      "org.mockito.internal.creation.jmock.ClassImposterizer",
      "org.objenesis.ObjenesisStd",
      "org.mockito.internal.progress.MockingProgress",
      "org.mockito.internal.util.MockitoLogger",
      "org.mockito.exceptions.misusing.MissingMethodInvocationException",
      "org.mockito.exceptions.verification.SmartNullPointerException",
      "org.mockito.exceptions.verification.TooLittleActualInvocations",
      "org.mockito.internal.progress.MockingProgressImpl",
      "org.mockito.internal.invocation.StubInfo",
      "org.mockito.internal.configuration.GlobalConfiguration",
      "org.objenesis.Objenesis",
      "org.mockito.stubbing.DeprecatedOngoingStubbing",
      "org.mockito.cglib.proxy.MethodInterceptor",
      "org.mockito.exceptions.verification.TooManyActualInvocations",
      "org.objenesis.strategy.InstantiatorStrategy",
      "org.mockito.exceptions.misusing.MockitoConfigurationException",
      "org.mockito.cglib.core.AbstractClassGenerator",
      "org.mockito.internal.progress.IOngoingStubbing",
      "org.mockito.internal.invocation.MockitoMethod",
      "org.mockitousage.basicapi.AnnotationsTest$NotAMock",
      "org.mockito.cglib.proxy.MethodProxy$CreateInfo",
      "org.mockito.internal.debugging.Localized",
      "org.mockito.Mock",
      "org.mockitousage.configuration.SmartMock",
      "org.mockito.internal.MockitoCore",
      "org.mockito.internal.util.ListUtil$Filter",
      "org.mockito.internal.invocation.SerializableMethod",
      "org.mockito.cglib.proxy.MethodProxy$FastClassInfo",
      "org.mockito.stubbing.Stubber",
      "org.mockito.asm.Type",
      "org.mockito.stubbing.Answer",
      "org.mockito.internal.verification.Times",
      "org.mockito.internal.exceptions.base.StackTraceFilter",
      "org.mockito.invocation.InvocationOnMock",
      "org.mockito.internal.progress.ArgumentMatcherStorageImpl",
      "org.mockito.cglib.proxy.Enhancer",
      "org.mockito.internal.verification.InOrderWrapper",
      "org.mockito.internal.verification.NoMoreInteractions",
      "org.objenesis.instantiator.ObjectInstantiator",
      "org.objenesis.strategy.StdInstantiatorStrategy",
      "org.mockito.configuration.IMockitoConfiguration",
      "org.mockito.exceptions.misusing.WrongTypeOfReturnValue",
      "org.mockito.stubbing.OngoingStubbing",
      "org.mockito.MockSettings",
      "org.mockito.internal.progress.ArgumentMatcherStorage",
      "org.mockito.exceptions.base.MockitoAssertionError",
      "org.mockito.internal.creation.jmock.ClassImposterizer$1",
      "org.mockito.internal.creation.jmock.ClassImposterizer$2",
      "org.mockito.internal.invocation.InvocationMarker",
      "org.mockito.exceptions.verification.NeverWantedButInvoked",
      "org.mockito.cglib.core.CodeGenerationException",
      "org.mockito.internal.util.CreationValidator",
      "org.hamcrest.Matcher",
      "org.mockito.internal.util.MockName",
      "org.mockito.internal.creation.SerializableMockitoMethodProxy",
      "org.objenesis.strategy.BaseInstantiatorStrategy",
      "org.mockito.configuration.AnnotationEngine",
      "org.mockito.internal.stubbing.StubbedInvocationMatcher",
      "org.mockito.exceptions.Reporter",
      "org.mockito.exceptions.verification.VerificationInOrderFailure",
      "org.mockito.configuration.DefaultMockitoConfiguration",
      "org.mockito.cglib.reflect.FastClass",
      "org.mockito.exceptions.misusing.NullInsteadOfMockException",
      "org.mockito.stubbing.VoidMethodStubbable",
      "org.mockito.internal.creation.DelegatingMockitoMethodProxy",
      "org.mockito.internal.util.StringJoiner",
      "org.mockito.stubbing.ClonesArguments",
      "org.mockito.cglib.core.NamingPolicy",
      "org.mockito.internal.verification.RegisteredInvocations",
      "org.mockitoutil.TestBase",
      "org.mockito.internal.verification.AtMost",
      "org.mockito.internal.creation.MockitoMethodProxy",
      "org.mockito.internal.reporting.PrintingFriendlyInvocation",
      "org.mockito.internal.creation.cglib.MockitoNamingPolicy",
      "org.mockito.exceptions.verification.ArgumentsAreDifferent",
      "org.hamcrest.SelfDescribing",
      "org.mockitousage.basicapi.AnnotationsTest$SuperBase",
      "org.mockito.configuration.MockitoConfiguration",
      "org.mockito.exceptions.misusing.InvalidUseOfMatchersException",
      "org.mockito.exceptions.misusing.UnfinishedStubbingException",
      "org.mockito.internal.invocation.Invocation",
      "org.mockito.internal.debugging.DebuggingInfo",
      "org.mockito.cglib.core.DefaultNamingPolicy",
      "org.mockito.exceptions.verification.NoInteractionsWanted",
      "org.mockito.internal.creation.AbstractMockitoMethodProxy",
      "org.mockito.internal.invocation.CapturesArgumensFromInvocation",
      "org.mockito.exceptions.PrintableInvocation",
      "org.mockito.internal.configuration.ClassPathLoader",
      "org.mockito.exceptions.base.MockitoException",
      "org.mockito.internal.verification.VerificationDataImpl",
      "org.mockito.internal.verification.api.VerificationData",
      "org.mockito.internal.invocation.realmethod.HasCGLIBMethodProxy",
      "org.mockito.internal.exceptions.base.ConditionalStackTraceFilter",
      "org.mockito.exceptions.verification.WantedButNotInvoked",
      "org.mockito.internal.verification.Only",
      "org.mockito.internal.progress.ThreadSafeMockingProgress",
      "org.mockito.internal.stubbing.ConsecutiveStubbing",
      "org.hamcrest.Factory",
      "org.mockito.internal.verification.api.VerificationMode",
      "org.mockito.internal.invocation.InvocationsFinder",
      "org.mockito.cglib.core.Signature",
      "org.mockito.internal.stubbing.BaseStubbing",
      "org.mockito.internal.debugging.Location",
      "org.mockito.cglib.proxy.CallbackFilter",
      "org.mockito.InOrder",
      "org.mockito.internal.MockitoInvocationHandler",
      "org.mockitousage.basicapi.AnnotationsTest$Sub",
      "org.mockito.cglib.proxy.MethodProxy",
      "org.mockito.internal.util.MockUtil",
      "org.objenesis.ObjenesisBase",
      "org.mockitousage.basicapi.AnnotationsTest",
      "org.mockito.internal.verification.api.VerificationInOrderMode",
      "org.mockito.internal.invocation.realmethod.CGLIBProxyRealMethod",
      "org.mockito.internal.reporting.PrintSettings",
      "org.mockito.cglib.core.ClassGenerator"
    );
  } 
}
