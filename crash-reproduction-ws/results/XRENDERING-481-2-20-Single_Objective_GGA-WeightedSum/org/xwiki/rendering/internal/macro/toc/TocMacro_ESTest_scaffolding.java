/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 17:47:10 UTC 2020
 */

package org.xwiki.rendering.internal.macro.toc;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class TocMacro_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.rendering.internal.macro.toc.TocMacro"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(TocMacro_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.rendering.parser.ParseException",
      "org.xwiki.rendering.macro.MacroExecutionException",
      "org.xwiki.xml.internal.html.DefaultHTMLCleanerConfiguration",
      "org.xwiki.rendering.block.XDOM",
      "org.xwiki.component.phase.Initializable",
      "org.xwiki.rendering.block.match.BlockNavigator$1",
      "org.xwiki.rendering.listener.reference.DocumentResourceReference",
      "org.xwiki.component.phase.InitializationException",
      "org.xwiki.rendering.block.BlockFilter",
      "org.xwiki.rendering.internal.macro.toc.TocMacro",
      "org.xwiki.rendering.syntax.Syntax",
      "org.xwiki.component.annotation.Role",
      "org.xwiki.rendering.wiki.WikiModelException",
      "org.xwiki.rendering.transformation.TransformationContext",
      "org.xwiki.rendering.parser.Parser",
      "org.xwiki.rendering.block.MetaDataBlock",
      "org.xwiki.component.descriptor.ComponentDescriptor",
      "org.xwiki.properties.PropertyException",
      "org.xwiki.rendering.macro.AbstractMacro",
      "org.xwiki.rendering.macro.descriptor.MacroDescriptor",
      "org.xwiki.rendering.internal.macro.toc.TreeParameters",
      "org.xwiki.rendering.wiki.WikiModel",
      "org.xwiki.properties.BeanManager",
      "org.xwiki.rendering.block.Block$Axes",
      "org.xwiki.rendering.renderer.reference.link.LinkLabelGenerator",
      "org.xwiki.rendering.listener.Listener",
      "org.xwiki.rendering.block.AbstractBlock",
      "org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder",
      "org.xwiki.rendering.block.TableCellBlock",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.rendering.listener.reference.ResourceReference",
      "org.xwiki.rendering.macro.toc.TocMacroParameters",
      "org.xwiki.rendering.block.match.BlockNavigator",
      "org.xwiki.rendering.block.Block",
      "org.xwiki.component.descriptor.ComponentDependency",
      "org.xwiki.rendering.block.MacroMarkerBlock",
      "org.xwiki.rendering.block.MacroBlock",
      "org.xwiki.rendering.block.match.BlockMatcher",
      "org.xwiki.xml.html.HTMLCleanerConfiguration",
      "org.xwiki.properties.annotation.PropertyDescription",
      "org.xwiki.rendering.block.CompatibilityBlock",
      "org.xwiki.rendering.listener.ImageListener",
      "org.xwiki.rendering.transformation.MacroTransformationContext",
      "org.xwiki.rendering.transformation.Transformation",
      "org.xwiki.rendering.macro.toc.TocMacroParameters$Scope",
      "org.xwiki.component.descriptor.ComponentInstantiationStrategy",
      "org.xwiki.rendering.listener.LinkListener",
      "org.xwiki.rendering.block.match.ClassBlockMatcher",
      "org.xwiki.rendering.macro.Macro",
      "org.xwiki.component.descriptor.ComponentRole",
      "org.xwiki.rendering.internal.macro.toc.TocTreeBuilder",
      "org.xwiki.rendering.block.SectionBlock",
      "org.xwiki.properties.BeanDescriptor",
      "org.xwiki.rendering.macro.descriptor.ContentDescriptor"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("javax.inject.Provider", false, TocMacro_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.component.descriptor.ComponentDescriptor", false, TocMacro_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.properties.BeanManager", false, TocMacro_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.rendering.macro.toc.TocMacroParameters", false, TocMacro_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.rendering.parser.Parser", false, TocMacro_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.rendering.renderer.reference.link.LinkLabelGenerator", false, TocMacro_ESTest_scaffolding.class.getClassLoader()));
  }
}
