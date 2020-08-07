/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat May 16 12:38:39 UTC 2020
 */

package org.mockito.internal.invocation;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class Invocation_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.mockito.internal.invocation.Invocation"; 
    org.evosuite.runtime.GuiSupport.initialize(); 
    org.evosuite.runtime.RuntimeSettings.maxNumberOfIterationsPerLoop = 10000; 
    org.evosuite.runtime.RuntimeSettings.mockSystemIn = true; 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    try { initMocksToAvoidTimeoutsInTheTests(); } catch(ClassNotFoundException e) {} 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(Invocation_ESTest_scaffolding.class.getClassLoader() ,
      "org.mockito.cglib.core.MethodInfo",
      "org.mockito.internal.matchers.ContainsExtraTypeInformation",
      "org.mockito.cglib.proxy.Callback",
      "org.hamcrest.StringDescription",
      "org.mockito.cglib.proxy.MethodInterceptorGenerator$1",
      "org.mockito.internal.util.ObjectMethodsGuru",
      "org.hamcrest.core.Is",
      "org.mockito.cglib.core.LocalVariablesSorter",
      "org.mockito.cglib.core.ClassNameReader$1",
      "org.mockito.cglib.core.ReflectUtils",
      "org.mockito.internal.invocation.realmethod.FilteredCGLIBProxyRealMethod",
      "org.mockito.cglib.proxy.CallbackGenerator",
      "org.mockito.cglib.core.ClassInfo",
      "org.mockito.cglib.core.ObjectSwitchCallback",
      "org.mockito.internal.invocation.realmethod.RealMethod",
      "org.mockito.exceptions.misusing.NotAMockException",
      "org.mockito.cglib.core.ClassEmitter$FieldInfo",
      "org.mockito.cglib.core.GeneratorStrategy",
      "org.mockito.cglib.core.Local",
      "org.mockito.cglib.core.ClassNameReader$EarlyExitException",
      "org.hamcrest.core.DescribedAs",
      "org.mockito.internal.configuration.GlobalConfiguration",
      "org.mockito.cglib.core.MethodWrapper",
      "org.mockito.cglib.core.MethodInfoTransformer",
      "org.mockito.cglib.core.DuplicatesPredicate",
      "org.mockito.cglib.proxy.MethodInterceptor",
      "org.mockito.cglib.reflect.FastClass$Generator",
      "org.mockito.cglib.proxy.CallbackGenerator$Context",
      "org.mockito.asm.Item",
      "org.mockito.asm.FieldVisitor",
      "org.mockito.cglib.core.ClassEmitter",
      "org.mockito.internal.invocation.SerializableMockitoMethod",
      "org.mockito.cglib.core.Transformer",
      "org.mockito.cglib.core.AbstractClassGenerator",
      "org.mockito.exceptions.misusing.MockitoConfigurationException",
      "org.mockito.internal.invocation.MockitoMethod",
      "org.mockito.cglib.proxy.MethodProxy$CreateInfo",
      "org.hamcrest.BaseDescription",
      "org.hamcrest.core.IsNot",
      "org.mockito.cglib.core.CodeEmitter$State",
      "org.mockito.cglib.proxy.MethodProxy$FastClassInfo",
      "org.mockito.cglib.core.KeyFactory$Generator",
      "org.hamcrest.core.AllOf",
      "org.mockito.asm.Type",
      "org.mockito.cglib.core.EmitUtils$15",
      "org.hamcrest.BaseMatcher",
      "org.mockito.internal.exceptions.base.StackTraceFilter",
      "org.mockito.cglib.core.ClassEmitter$3",
      "org.mockito.stubbing.Answer",
      "org.mockito.cglib.core.EmitUtils$16",
      "org.mockito.invocation.InvocationOnMock",
      "org.mockito.cglib.proxy.Enhancer",
      "org.mockito.cglib.core.EmitUtils$13",
      "org.mockito.cglib.core.EmitUtils$14",
      "org.mockito.cglib.core.EmitUtils$11",
      "org.mockito.cglib.core.EmitUtils$12",
      "org.mockito.cglib.core.ProcessArrayCallback",
      "org.mockito.cglib.core.EmitUtils$10",
      "org.hamcrest.core.IsEqual",
      "org.mockito.asm.Opcodes",
      "org.hamcrest.core.IsSame",
      "org.mockito.cglib.core.ClassEmitter$1",
      "org.mockito.cglib.core.VisibilityPredicate",
      "org.mockito.configuration.IMockitoConfiguration",
      "org.mockito.cglib.core.Predicate",
      "org.mockito.cglib.core.EmitUtils$ArrayDelimiters",
      "org.mockito.cglib.reflect.FastClassEmitter",
      "org.mockito.internal.matchers.Equals",
      "org.mockito.asm.ClassVisitor",
      "org.mockito.cglib.core.CodeGenerationException",
      "org.mockito.internal.util.CreationValidator",
      "org.mockito.cglib.core.CollectionUtils",
      "org.hamcrest.Matcher",
      "org.mockito.asm.MethodAdapter",
      "org.mockito.internal.creation.SerializableMockitoMethodProxy",
      "org.mockito.cglib.core.KeyFactory$2",
      "org.mockito.cglib.core.KeyFactory$1",
      "org.hamcrest.core.IsNull",
      "org.mockito.cglib.core.Customizer",
      "org.mockito.configuration.AnnotationEngine",
      "org.mockito.asm.Handler",
      "org.mockito.cglib.core.EmitUtils",
      "org.mockito.cglib.core.Block",
      "org.mockito.cglib.core.Constants",
      "org.mockito.configuration.DefaultMockitoConfiguration",
      "org.mockito.cglib.reflect.FastClass",
      "org.mockito.ArgumentMatcher",
      "org.mockito.cglib.core.DebuggingClassWriter",
      "org.mockito.internal.creation.DelegatingMockitoMethodProxy",
      "org.mockito.cglib.core.NamingPolicy",
      "org.mockito.cglib.core.LocalVariablesSorter$State",
      "org.mockito.cglib.core.ReflectUtils$4",
      "org.mockito.cglib.core.ReflectUtils$2",
      "org.mockito.cglib.core.ReflectUtils$3",
      "org.mockito.asm.ByteVector",
      "org.mockito.cglib.core.ReflectUtils$1",
      "org.mockito.internal.creation.MockitoMethodProxy",
      "org.mockito.cglib.core.DebuggingClassWriter$1",
      "org.mockito.internal.reporting.PrintingFriendlyInvocation",
      "org.mockito.internal.creation.cglib.MockitoNamingPolicy",
      "org.mockito.cglib.reflect.FastClassEmitter$3",
      "org.mockito.cglib.reflect.FastClassEmitter$4",
      "org.mockito.cglib.core.AbstractClassGenerator$1",
      "org.mockito.cglib.core.DefaultGeneratorStrategy",
      "org.mockito.cglib.reflect.FastClassEmitter$1",
      "org.mockito.cglib.core.MethodWrapper$MethodWrapperKey",
      "org.mockito.cglib.core.ProcessSwitchCallback",
      "org.mockito.cglib.core.ClassNameReader",
      "org.hamcrest.SelfDescribing",
      "org.mockito.cglib.core.AbstractClassGenerator$Source",
      "org.mockito.asm.FieldWriter",
      "org.mockito.configuration.MockitoConfiguration",
      "org.mockito.cglib.proxy.MethodInterceptorGenerator",
      "org.mockito.internal.invocation.Invocation",
      "org.mockito.cglib.core.EmitUtils$ParameterTyper",
      "org.mockito.cglib.core.DefaultNamingPolicy",
      "org.hamcrest.internal.SelfDescribingValue",
      "org.mockito.cglib.core.TypeUtils",
      "org.mockito.internal.creation.AbstractMockitoMethodProxy",
      "org.mockito.cglib.core.CodeEmitter",
      "org.mockito.exceptions.PrintableInvocation",
      "org.mockito.asm.ClassReader",
      "org.mockito.internal.configuration.ClassPathLoader",
      "org.mockito.exceptions.base.MockitoException",
      "org.mockito.internal.invocation.realmethod.HasCGLIBMethodProxy",
      "org.mockito.internal.exceptions.base.ConditionalStackTraceFilter",
      "org.hamcrest.core.AnyOf",
      "org.mockito.internal.util.Primitives",
      "org.mockito.asm.MethodWriter",
      "org.hamcrest.core.IsAnything",
      "org.hamcrest.Factory",
      "org.mockito.asm.Edge",
      "org.mockito.cglib.core.RejectModifierPredicate",
      "org.mockito.asm.Label",
      "org.mockito.internal.IMockHandler",
      "org.hamcrest.core.IsInstanceOf",
      "org.mockito.cglib.core.Signature",
      "org.hamcrest.Description",
      "org.mockito.internal.debugging.Location",
      "org.mockito.cglib.core.EmitUtils$5",
      "org.mockito.cglib.proxy.CallbackFilter",
      "org.mockito.asm.Attribute",
      "org.mockito.cglib.core.EmitUtils$8",
      "org.mockito.asm.AnnotationVisitor",
      "org.mockito.cglib.core.EmitUtils$9",
      "org.mockito.asm.ClassAdapter",
      "org.mockito.cglib.core.EmitUtils$6",
      "org.mockito.cglib.core.EmitUtils$7",
      "org.mockito.cglib.reflect.FastClassEmitter$GetIndexCallback",
      "org.mockito.cglib.proxy.MethodProxy",
      "org.mockito.internal.util.MockUtil",
      "org.mockito.cglib.proxy.Enhancer$EnhancerKey",
      "org.mockito.cglib.proxy.Enhancer$1",
      "org.mockito.asm.MethodVisitor",
      "org.mockito.asm.Frame",
      "org.mockito.asm.ClassWriter",
      "org.mockito.internal.matchers.MatchersPrinter",
      "org.mockito.cglib.core.KeyFactory",
      "org.mockito.internal.invocation.realmethod.CGLIBProxyRealMethod",
      "org.mockito.internal.reporting.PrintSettings",
      "org.mockito.cglib.core.ClassGenerator"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.mockito.internal.invocation.MockitoMethod", false, Invocation_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.mockito.invocation.InvocationOnMock", false, Invocation_ESTest_scaffolding.class.getClassLoader()));
  }
}
