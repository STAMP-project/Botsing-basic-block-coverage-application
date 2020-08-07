/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat May 16 23:34:54 UTC 2020
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
      "org.mockito.internal.stubbing.defaultanswers.ReturnsSmartNulls",
      "org.mockito.internal.invocation.InvocationMatcher",
      "org.mockito.internal.verification.AtLeast",
      "org.mockito.cglib.core.LocalVariablesSorter",
      "org.mockito.cglib.core.ClassNameReader$1",
      "org.mockito.cglib.core.ReflectUtils",
      "org.mockito.internal.debugging.MockitoDebuggerImpl",
      "org.mockito.internal.matchers.InstanceOf",
      "org.mockito.internal.invocation.realmethod.FilteredCGLIBProxyRealMethod",
      "org.mockito.cglib.core.ClassInfo",
      "org.mockito.cglib.core.ObjectSwitchCallback",
      "org.mockito.exceptions.misusing.NotAMockException",
      "org.mockito.cglib.core.ClassEmitter$FieldInfo",
      "org.mockito.internal.stubbing.defaultanswers.ReturnsMoreEmptyValues",
      "org.mockito.exceptions.verification.SmartNullPointerException",
      "org.mockito.internal.stubbing.defaultanswers.ReturnsMocks",
      "org.mockito.internal.progress.MockingProgressImpl",
      "org.mockito.cglib.core.Local",
      "org.mockito.cglib.core.ClassNameReader$EarlyExitException",
      "org.mockito.internal.configuration.GlobalConfiguration",
      "org.mockito.cglib.core.MethodWrapper",
      "org.mockito.cglib.core.MethodInfoTransformer",
      "org.mockito.cglib.core.DuplicatesPredicate",
      "org.mockito.cglib.proxy.CallbackGenerator$Context",
      "org.mockito.asm.Item",
      "org.mockito.cglib.core.Transformer",
      "org.mockito.exceptions.misusing.MockitoConfigurationException",
      "org.mockito.internal.progress.IOngoingStubbing",
      "org.mockito.internal.verification.VerificationModeFactory",
      "org.mockito.cglib.proxy.MethodProxy$CreateInfo",
      "org.hamcrest.BaseDescription",
      "org.hamcrest.core.IsNot",
      "org.mockito.cglib.core.CodeEmitter$State",
      "org.mockito.internal.MockitoCore",
      "org.mockito.cglib.core.KeyFactory$Generator",
      "org.mockito.stubbing.Stubber",
      "org.hamcrest.core.AllOf",
      "org.mockito.asm.Type",
      "org.mockito.cglib.core.EmitUtils$15",
      "org.mockito.stubbing.Answer",
      "org.mockito.cglib.core.ClassEmitter$3",
      "org.mockito.cglib.core.EmitUtils$16",
      "org.mockito.internal.matchers.Any",
      "org.mockito.internal.progress.ArgumentMatcherStorageImpl",
      "org.mockito.cglib.proxy.Enhancer",
      "org.mockito.cglib.core.EmitUtils$13",
      "org.mockito.cglib.core.EmitUtils$14",
      "org.mockito.cglib.core.EmitUtils$11",
      "org.mockito.cglib.core.EmitUtils$12",
      "org.mockito.cglib.core.ProcessArrayCallback",
      "org.mockito.cglib.core.EmitUtils$10",
      "org.hamcrest.core.IsEqual",
      "org.mockito.internal.verification.NoMoreInteractions",
      "org.mockito.asm.Opcodes",
      "org.mockito.cglib.core.ClassEmitter$1",
      "org.mockito.cglib.core.VisibilityPredicate",
      "org.mockito.exceptions.misusing.WrongTypeOfReturnValue",
      "org.mockito.MockSettings",
      "org.mockito.cglib.core.EmitUtils$ArrayDelimiters",
      "org.mockito.cglib.reflect.FastClassEmitter",
      "org.mockito.exceptions.base.MockitoAssertionError",
      "org.mockito.internal.invocation.InvocationMarker",
      "org.mockito.asm.ClassVisitor",
      "org.mockito.exceptions.verification.NeverWantedButInvoked",
      "org.mockito.internal.util.CreationValidator",
      "org.mockito.cglib.core.CodeGenerationException",
      "org.mockito.cglib.core.CollectionUtils",
      "org.hamcrest.Matcher",
      "org.mockito.asm.MethodAdapter",
      "org.mockito.internal.util.MockName",
      "org.mockito.internal.creation.SerializableMockitoMethodProxy",
      "org.mockito.cglib.core.KeyFactory$2",
      "org.mockito.cglib.core.KeyFactory$1",
      "org.mockito.cglib.core.Customizer",
      "org.mockito.cglib.core.Block",
      "org.mockito.exceptions.Reporter",
      "org.mockito.cglib.reflect.FastClass",
      "org.mockito.internal.stubbing.answers.DoesNothing",
      "org.mockito.stubbing.VoidMethodStubbable",
      "org.mockito.ArgumentMatcher",
      "org.mockito.cglib.core.DebuggingClassWriter",
      "org.mockito.cglib.core.NamingPolicy",
      "org.mockito.cglib.core.LocalVariablesSorter$State",
      "org.mockito.internal.verification.AtMost",
      "org.mockito.cglib.core.DebuggingClassWriter$1",
      "org.mockito.exceptions.verification.ArgumentsAreDifferent",
      "org.hamcrest.SelfDescribing",
      "org.mockito.asm.FieldWriter",
      "org.mockito.configuration.MockitoConfiguration",
      "org.mockito.exceptions.misusing.InvalidUseOfMatchersException",
      "org.mockito.internal.invocation.Invocation",
      "org.mockito.internal.debugging.DebuggingInfo",
      "org.mockito.cglib.core.DefaultNamingPolicy",
      "org.hamcrest.internal.SelfDescribingValue",
      "org.mockito.cglib.core.CodeEmitter",
      "org.mockito.internal.configuration.ClassPathLoader",
      "org.mockito.exceptions.base.MockitoException",
      "org.mockito.internal.invocation.realmethod.HasCGLIBMethodProxy",
      "org.mockito.internal.exceptions.base.ConditionalStackTraceFilter",
      "org.mockito.internal.stubbing.StubberImpl",
      "org.mockito.internal.verification.Only",
      "org.mockito.asm.Edge",
      "org.mockito.asm.Label",
      "org.mockito.cglib.core.Signature",
      "org.mockito.internal.debugging.Location",
      "org.mockito.cglib.core.EmitUtils$5",
      "org.mockito.cglib.proxy.CallbackFilter",
      "org.mockito.cglib.core.EmitUtils$8",
      "org.mockito.cglib.core.EmitUtils$9",
      "org.mockito.cglib.core.EmitUtils$6",
      "org.mockito.cglib.core.EmitUtils$7",
      "org.mockito.internal.matchers.Null",
      "org.mockito.asm.Frame",
      "org.mockito.asm.ClassWriter",
      "org.mockito.internal.matchers.VarargMatcher",
      "org.mockito.internal.invocation.realmethod.CGLIBProxyRealMethod",
      "org.mockito.cglib.core.MethodInfo",
      "org.mockito.internal.matchers.ContainsExtraTypeInformation",
      "org.mockito.cglib.proxy.Callback",
      "org.mockito.exceptions.misusing.UnfinishedVerificationException",
      "org.mockito.internal.stubbing.defaultanswers.GloballyConfiguredAnswer",
      "org.hamcrest.StringDescription",
      "org.mockito.internal.creation.MockSettingsImpl",
      "org.hamcrest.core.Is",
      "org.mockito.internal.invocation.realmethod.RealMethod",
      "org.mockito.cglib.core.GeneratorStrategy",
      "org.mockito.internal.progress.MockingProgress",
      "org.mockito.internal.util.MockitoLogger",
      "org.mockito.exceptions.misusing.MissingMethodInvocationException",
      "org.mockito.internal.stubbing.answers.CallsRealMethods",
      "org.mockito.exceptions.verification.TooLittleActualInvocations",
      "org.mockito.internal.invocation.StubInfo",
      "org.hamcrest.core.DescribedAs",
      "org.mockito.cglib.proxy.MethodInterceptor",
      "org.mockito.stubbing.DeprecatedOngoingStubbing",
      "org.mockito.exceptions.verification.TooManyActualInvocations",
      "org.mockito.cglib.reflect.FastClass$Generator",
      "org.mockito.asm.FieldVisitor",
      "org.mockito.cglib.core.ClassEmitter",
      "org.mockito.cglib.core.AbstractClassGenerator",
      "org.mockito.internal.invocation.MockitoMethod",
      "org.mockito.internal.util.ListUtil$Filter",
      "org.mockito.internal.invocation.SerializableMethod",
      "org.mockito.cglib.proxy.MethodProxy$FastClassInfo",
      "org.hamcrest.BaseMatcher",
      "org.mockito.internal.verification.Times",
      "org.mockito.internal.exceptions.base.StackTraceFilter",
      "org.mockito.invocation.InvocationOnMock",
      "org.mockito.internal.verification.InOrderWrapper",
      "org.hamcrest.core.IsSame",
      "org.mockito.configuration.IMockitoConfiguration",
      "org.mockito.MockitoDebugger",
      "org.mockito.stubbing.OngoingStubbing",
      "org.mockito.cglib.core.Predicate",
      "org.mockito.internal.progress.ArgumentMatcherStorage",
      "org.mockito.internal.matchers.LocalizedMatcher",
      "org.hamcrest.core.IsNull",
      "org.mockito.configuration.AnnotationEngine",
      "org.mockito.asm.Handler",
      "org.mockito.cglib.core.EmitUtils",
      "org.mockito.cglib.core.Constants",
      "org.mockito.internal.stubbing.StubbedInvocationMatcher",
      "org.mockito.exceptions.verification.VerificationInOrderFailure",
      "org.mockito.configuration.DefaultMockitoConfiguration",
      "org.mockito.exceptions.misusing.NullInsteadOfMockException",
      "org.mockito.ReturnValues",
      "org.mockito.internal.creation.DelegatingMockitoMethodProxy",
      "org.mockito.internal.util.StringJoiner",
      "org.mockito.stubbing.ClonesArguments",
      "org.mockito.cglib.core.ReflectUtils$4",
      "org.mockito.cglib.core.ReflectUtils$2",
      "org.mockito.cglib.core.ReflectUtils$3",
      "org.mockito.asm.ByteVector",
      "org.mockito.cglib.core.ReflectUtils$1",
      "org.mockito.internal.matchers.NotNull",
      "org.mockito.internal.matchers.MatcherDecorator",
      "org.mockito.internal.creation.MockitoMethodProxy",
      "org.mockito.internal.reporting.PrintingFriendlyInvocation",
      "org.mockito.cglib.reflect.FastClassEmitter$3",
      "org.mockito.cglib.reflect.FastClassEmitter$4",
      "org.mockito.cglib.core.AbstractClassGenerator$1",
      "org.mockito.cglib.core.DefaultGeneratorStrategy",
      "org.mockito.cglib.reflect.FastClassEmitter$1",
      "org.mockito.cglib.core.MethodWrapper$MethodWrapperKey",
      "org.mockito.cglib.core.ProcessSwitchCallback",
      "org.mockito.cglib.core.ClassNameReader",
      "org.mockito.cglib.core.AbstractClassGenerator$Source",
      "org.mockito.exceptions.misusing.UnfinishedStubbingException",
      "org.mockito.cglib.core.EmitUtils$ParameterTyper",
      "org.mockito.Matchers",
      "org.mockito.exceptions.verification.NoInteractionsWanted",
      "org.mockito.cglib.core.TypeUtils",
      "org.mockito.internal.creation.AbstractMockitoMethodProxy",
      "org.mockito.internal.invocation.CapturesArgumensFromInvocation",
      "org.mockito.exceptions.PrintableInvocation",
      "org.mockito.asm.ClassReader",
      "org.mockito.internal.verification.VerificationDataImpl",
      "org.mockito.internal.matchers.CapturesArguments",
      "org.mockito.internal.verification.api.VerificationData",
      "org.hamcrest.core.AnyOf",
      "org.mockito.internal.util.Primitives",
      "org.mockito.internal.progress.ThreadSafeMockingProgress",
      "org.mockito.exceptions.verification.WantedButNotInvoked",
      "org.mockito.asm.MethodWriter",
      "org.hamcrest.core.IsAnything",
      "org.hamcrest.Factory",
      "org.mockito.internal.verification.api.VerificationMode",
      "org.hamcrest.core.IsInstanceOf",
      "org.mockito.internal.invocation.InvocationsFinder",
      "org.hamcrest.Description",
      "org.mockito.asm.Attribute",
      "org.mockito.InOrder",
      "org.mockito.internal.MockitoInvocationHandler",
      "org.mockito.asm.AnnotationVisitor",
      "org.mockito.asm.ClassAdapter",
      "org.mockito.internal.stubbing.defaultanswers.ReturnsEmptyValues",
      "org.mockito.cglib.reflect.FastClassEmitter$GetIndexCallback",
      "org.mockito.internal.progress.HandyReturnValues",
      "org.mockito.cglib.proxy.MethodProxy",
      "org.mockito.Mockito",
      "org.mockito.internal.util.MockUtil",
      "org.mockito.cglib.proxy.Enhancer$EnhancerKey",
      "org.mockito.cglib.proxy.Enhancer$1",
      "org.mockito.internal.matchers.AnyVararg",
      "org.mockito.asm.MethodVisitor",
      "org.mockito.internal.verification.api.VerificationInOrderMode",
      "org.mockito.cglib.core.KeyFactory",
      "org.mockito.internal.reporting.PrintSettings",
      "org.mockito.cglib.core.ClassGenerator"
    );
  } 
}
