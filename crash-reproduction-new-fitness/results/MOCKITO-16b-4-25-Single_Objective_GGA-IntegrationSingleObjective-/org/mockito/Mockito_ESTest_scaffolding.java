/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 03:59:19 UTC 2020
 */

package org.mockito;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class Mockito_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.mockito.Mockito"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(Mockito_ESTest_scaffolding.class.getClassLoader() ,
      "org.mockito.internal.matchers.ContainsExtraTypeInformation",
      "org.mockito.cglib.proxy.Callback",
      "org.mockito.exceptions.misusing.UnfinishedVerificationException",
      "org.mockito.internal.stubbing.defaultanswers.GloballyConfiguredAnswer",
      "org.hamcrest.StringDescription",
      "org.mockito.internal.stubbing.defaultanswers.ReturnsSmartNulls",
      "org.mockito.internal.invocation.InvocationMatcher",
      "org.mockito.internal.creation.MockSettingsImpl",
      "org.mockito.internal.verification.AtLeast",
      "org.hamcrest.core.Is",
      "org.mockito.internal.creation.jmock.ClassImposterizer$3",
      "org.mockito.internal.invocation.realmethod.FilteredCGLIBProxyRealMethod",
      "org.mockito.exceptions.misusing.NotAMockException",
      "org.mockito.internal.invocation.realmethod.RealMethod",
      "org.mockito.internal.creation.jmock.ClassImposterizer",
      "org.mockito.internal.stubbing.defaultanswers.ReturnsMoreEmptyValues",
      "org.objenesis.ObjenesisStd",
      "org.mockito.internal.progress.MockingProgress",
      "org.mockito.internal.util.MockitoLogger",
      "org.mockito.exceptions.misusing.MissingMethodInvocationException",
      "org.mockito.exceptions.verification.SmartNullPointerException",
      "org.mockito.internal.stubbing.answers.CallsRealMethods",
      "org.mockito.exceptions.verification.TooLittleActualInvocations",
      "org.mockito.internal.stubbing.defaultanswers.ReturnsMocks",
      "org.mockito.internal.progress.MockingProgressImpl",
      "org.mockito.internal.invocation.StubInfo",
      "org.hamcrest.core.DescribedAs",
      "org.mockito.internal.configuration.GlobalConfiguration",
      "org.objenesis.Objenesis",
      "org.mockito.cglib.proxy.MethodInterceptor",
      "org.mockito.stubbing.DeprecatedOngoingStubbing",
      "org.mockito.exceptions.verification.TooManyActualInvocations",
      "org.objenesis.strategy.InstantiatorStrategy",
      "org.mockito.exceptions.misusing.MockitoConfigurationException",
      "org.mockito.cglib.core.AbstractClassGenerator",
      "org.mockito.internal.progress.IOngoingStubbing",
      "org.mockito.internal.invocation.MockitoMethod",
      "org.mockito.cglib.proxy.MethodProxy$CreateInfo",
      "org.hamcrest.BaseDescription",
      "org.hamcrest.core.IsNot",
      "org.mockito.internal.MockitoCore",
      "org.mockito.internal.invocation.SerializableMethod",
      "org.mockito.cglib.proxy.MethodProxy$FastClassInfo",
      "org.mockito.stubbing.Stubber",
      "org.hamcrest.core.AllOf",
      "org.mockito.asm.Type",
      "org.mockito.stubbing.Answer",
      "org.hamcrest.BaseMatcher",
      "org.mockito.internal.verification.Times",
      "org.mockito.internal.exceptions.base.StackTraceFilter",
      "org.mockito.invocation.InvocationOnMock",
      "org.mockito.internal.progress.ArgumentMatcherStorageImpl",
      "org.mockito.cglib.proxy.Enhancer",
      "org.mockito.internal.matchers.Any",
      "org.hamcrest.core.IsEqual",
      "org.mockito.internal.verification.InOrderWrapper",
      "org.mockito.internal.verification.NoMoreInteractions",
      "org.objenesis.instantiator.ObjectInstantiator",
      "org.objenesis.strategy.StdInstantiatorStrategy",
      "org.hamcrest.core.IsSame",
      "org.mockito.configuration.IMockitoConfiguration",
      "org.mockito.exceptions.misusing.WrongTypeOfReturnValue",
      "org.mockito.MockitoDebugger",
      "org.mockito.MockSettings",
      "org.mockito.stubbing.OngoingStubbing",
      "org.mockito.internal.progress.ArgumentMatcherStorage",
      "org.mockito.exceptions.base.MockitoAssertionError",
      "org.mockito.internal.creation.jmock.ClassImposterizer$1",
      "org.mockito.internal.creation.jmock.ClassImposterizer$2",
      "org.mockito.internal.invocation.InvocationMarker",
      "org.mockito.internal.matchers.LocalizedMatcher",
      "org.mockito.exceptions.verification.NeverWantedButInvoked",
      "org.mockito.internal.util.CreationValidator",
      "org.mockito.cglib.core.CodeGenerationException",
      "org.hamcrest.Matcher",
      "org.mockito.internal.util.MockName",
      "org.mockito.internal.creation.SerializableMockitoMethodProxy",
      "org.objenesis.strategy.BaseInstantiatorStrategy",
      "org.hamcrest.core.IsNull",
      "org.mockito.internal.stubbing.answers.ThrowsException",
      "org.mockito.configuration.AnnotationEngine",
      "org.mockito.internal.stubbing.StubbedInvocationMatcher",
      "org.mockito.exceptions.Reporter",
      "org.mockito.exceptions.verification.VerificationInOrderFailure",
      "org.mockito.configuration.DefaultMockitoConfiguration",
      "org.mockito.cglib.reflect.FastClass",
      "org.mockito.internal.stubbing.answers.DoesNothing",
      "org.mockito.exceptions.misusing.NullInsteadOfMockException",
      "org.mockito.stubbing.VoidMethodStubbable",
      "org.mockito.ArgumentMatcher",
      "org.mockito.ReturnValues",
      "org.mockito.internal.creation.DelegatingMockitoMethodProxy",
      "org.mockito.internal.util.StringJoiner",
      "org.mockito.stubbing.ClonesArguments",
      "org.mockito.cglib.core.NamingPolicy",
      "org.mockito.internal.matchers.MatcherDecorator",
      "org.mockito.internal.verification.AtMost",
      "org.mockito.internal.creation.MockitoMethodProxy",
      "org.mockito.internal.reporting.PrintingFriendlyInvocation",
      "org.mockito.internal.creation.cglib.MockitoNamingPolicy",
      "org.mockito.exceptions.verification.ArgumentsAreDifferent",
      "org.hamcrest.SelfDescribing",
      "org.mockito.configuration.MockitoConfiguration",
      "org.mockito.exceptions.misusing.InvalidUseOfMatchersException",
      "org.mockito.exceptions.misusing.UnfinishedStubbingException",
      "org.mockito.internal.invocation.Invocation",
      "org.mockito.internal.debugging.DebuggingInfo",
      "org.mockito.cglib.core.DefaultNamingPolicy",
      "org.mockito.Matchers",
      "org.mockito.exceptions.verification.NoInteractionsWanted",
      "org.hamcrest.internal.SelfDescribingValue",
      "org.mockito.internal.creation.AbstractMockitoMethodProxy",
      "org.mockito.internal.invocation.CapturesArgumensFromInvocation",
      "org.mockito.exceptions.PrintableInvocation",
      "org.mockito.internal.configuration.ClassPathLoader",
      "org.mockito.exceptions.base.MockitoException",
      "org.mockito.internal.verification.VerificationDataImpl",
      "org.mockito.internal.matchers.CapturesArguments",
      "org.mockito.internal.verification.api.VerificationData",
      "org.mockito.internal.invocation.realmethod.HasCGLIBMethodProxy",
      "org.mockito.internal.exceptions.base.ConditionalStackTraceFilter",
      "org.mockito.internal.stubbing.StubberImpl",
      "org.hamcrest.core.AnyOf",
      "org.mockito.internal.progress.ThreadSafeMockingProgress",
      "org.mockito.exceptions.verification.WantedButNotInvoked",
      "org.mockito.internal.verification.Only",
      "org.hamcrest.core.IsAnything",
      "org.hamcrest.Factory",
      "org.mockito.internal.verification.api.VerificationMode",
      "org.hamcrest.core.IsInstanceOf",
      "org.mockito.internal.invocation.InvocationsFinder",
      "org.mockito.cglib.core.Signature",
      "org.hamcrest.Description",
      "org.mockito.internal.debugging.Location",
      "org.mockito.cglib.proxy.CallbackFilter",
      "org.mockito.InOrder",
      "org.mockito.internal.MockitoInvocationHandler",
      "org.mockito.internal.stubbing.defaultanswers.ReturnsEmptyValues",
      "org.mockito.internal.progress.HandyReturnValues",
      "org.mockito.cglib.proxy.MethodProxy",
      "org.mockito.Mockito",
      "org.mockito.internal.util.MockUtil",
      "org.objenesis.ObjenesisBase",
      "org.mockito.internal.verification.api.VerificationInOrderMode",
      "org.mockito.internal.invocation.realmethod.CGLIBProxyRealMethod",
      "org.mockito.internal.reporting.PrintSettings",
      "org.mockito.cglib.core.ClassGenerator"
    );
  } 
}
