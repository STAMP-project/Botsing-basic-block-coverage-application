/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 16:30:44 UTC 2020
 */

package org.xwiki.rendering.internal.macro.toc;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class TreeParametersBuilder_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(TreeParametersBuilder_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.rendering.block.XDOM",
      "org.xwiki.component.internal.RoleHint",
      "org.xwiki.rendering.block.match.BlockNavigator$1",
      "org.xwiki.component.phase.Disposable",
      "org.xwiki.component.manager.ComponentLifecycleException",
      "org.xwiki.rendering.block.BlockFilter",
      "org.xwiki.component.embed.LifecycleHandler",
      "org.xwiki.component.descriptor.DefaultComponentDescriptor",
      "org.xwiki.rendering.renderer.xml.ContentHandlerBlockRenderer",
      "org.xwiki.text.XWikiToStringBuilder",
      "org.xwiki.rendering.syntax.Syntax",
      "org.xwiki.component.annotation.Role",
      "org.xwiki.rendering.renderer.BlockRenderer",
      "org.xwiki.component.manager.CompatibilityComponentManager",
      "org.xwiki.rendering.transformation.TransformationContext",
      "org.xwiki.component.manager.ComponentEventManager",
      "org.xwiki.rendering.block.MetaDataBlock",
      "org.xwiki.rendering.renderer.PrintRendererFactory",
      "org.xwiki.component.embed.EmbeddableComponentManager",
      "org.xwiki.component.descriptor.ComponentDescriptor",
      "org.xwiki.rendering.block.ExpandedMacroBlock",
      "org.xwiki.rendering.block.BulletedListBlock",
      "org.xwiki.rendering.internal.macro.toc.TreeParameters",
      "org.xwiki.rendering.xml.internal.renderer.AbstractRenderer",
      "org.xwiki.rendering.block.Block$Axes",
      "org.xwiki.component.manager.ComponentManager",
      "org.xwiki.rendering.listener.reference.ResourceType",
      "org.xwiki.component.descriptor.DefaultComponentRole",
      "org.xwiki.component.manager.NamespacedComponentManager",
      "org.xwiki.rendering.renderer.xml.ContentHandlerStreamRendererFactory",
      "org.apache.commons.lang3.builder.ToStringBuilder",
      "org.xwiki.rendering.internal.renderer.xwiki21.XWikiSyntaxBlockRenderer",
      "org.xwiki.rendering.internal.renderer.AbstractBlockRenderer",
      "org.xwiki.rendering.listener.Listener",
      "org.xwiki.rendering.renderer.PrintRenderer",
      "org.xwiki.component.embed.EmbeddableComponentManager$ComponentEntry",
      "org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder",
      "org.xwiki.rendering.block.AbstractBlock",
      "org.xwiki.filter.xml.serializer.XMLSerializerFactory",
      "org.xwiki.rendering.renderer.Renderer",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.rendering.listener.reference.ResourceReference",
      "org.xwiki.rendering.macro.toc.TocMacroParameters",
      "org.apache.commons.lang3.builder.ToStringStyle",
      "org.xwiki.component.manager.ComponentLookupException",
      "org.apache.commons.lang3.builder.StandardToStringStyle",
      "org.xwiki.component.manager.ComponentRepositoryException",
      "org.xwiki.rendering.block.match.BlockNavigator",
      "org.xwiki.rendering.block.Block",
      "org.xwiki.component.descriptor.ComponentDependency",
      "org.xwiki.filter.xml.XMLConfiguration",
      "org.xwiki.rendering.renderer.xml.ContentHandlerStreamRenderer",
      "org.xwiki.rendering.renderer.printer.WikiPrinter",
      "org.xwiki.rendering.xdomxmlcurrent.internal.renderer.XDOMXMLRenderer",
      "org.xwiki.rendering.block.match.BlockMatcher",
      "org.xwiki.rendering.block.MacroBlock",
      "org.xwiki.rendering.xml.internal.renderer.AbstractStreamRendererFactory",
      "org.xwiki.text.XWikiToStringStyle",
      "org.xwiki.rendering.block.CompatibilityBlock",
      "org.xwiki.rendering.block.ListBLock",
      "org.xwiki.rendering.listener.ImageListener",
      "org.xwiki.rendering.transformation.MacroTransformationContext",
      "org.xwiki.rendering.transformation.Transformation",
      "org.xwiki.rendering.macro.toc.TocMacroParameters$Scope",
      "org.xwiki.component.descriptor.ComponentInstantiationStrategy",
      "org.xwiki.rendering.listener.LinkListener",
      "org.apache.commons.lang3.builder.Builder",
      "org.xwiki.rendering.block.match.ClassBlockMatcher",
      "org.xwiki.component.descriptor.ComponentRole",
      "org.xwiki.rendering.block.SectionBlock"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.slf4j.Logger", false, TreeParametersBuilder_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.filter.xml.serializer.XMLSerializerFactory", false, TreeParametersBuilder_ESTest_scaffolding.class.getClassLoader()));
  }
}
