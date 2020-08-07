/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 01:42:30 UTC 2020
 */

package org.xwiki.rendering.internal.macro.toc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Map;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.rendering.block.Block;
import org.xwiki.rendering.block.MacroBlock;
import org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder;
import org.xwiki.rendering.listener.reference.ResourceReference;
import org.xwiki.rendering.listener.reference.ResourceType;
import org.xwiki.rendering.macro.toc.TocMacroParameters;
import org.xwiki.rendering.transformation.MacroTransformationContext;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class TreeParametersBuilder_ESTest extends TreeParametersBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TocMacroParameters.Scope tocMacroParameters_Scope0 = TocMacroParameters.Scope.PAGE;
      TocMacroParameters.Scope tocMacroParameters_Scope1 = TocMacroParameters.Scope.LOCAL;
      TocMacroParameters tocMacroParameters0 = mock(TocMacroParameters.class, new ViolatedAssumptionAnswer());
      doReturn((-2863)).when(tocMacroParameters0).getDepth();
      doReturn("@6)|cU").when(tocMacroParameters0).getReference();
      doReturn(tocMacroParameters_Scope0, tocMacroParameters_Scope1).when(tocMacroParameters0).getScope();
      doReturn((-2863)).when(tocMacroParameters0).getStart();
      doReturn(false).when(tocMacroParameters0).isCustomStart();
      doReturn(false).when(tocMacroParameters0).isNumbered();
      MacroTransformationContext macroTransformationContext0 = new MacroTransformationContext();
      ResourceType resourceType0 = ResourceType.URL;
      ResourceReference resourceReference0 = new ResourceReference("Uf%flDnlG", resourceType0);
      Map<String, String> map0 = resourceReference0.getParameters();
      MacroBlock macroBlock0 = new MacroBlock("", map0, "/i`4hn`H ]Up$db", false);
      macroTransformationContext0.setCurrentMacroBlock(macroBlock0);
      TreeParametersBuilder treeParametersBuilder0 = new TreeParametersBuilder();
      // Undeclared exception!
      treeParametersBuilder0.build((Block) null, tocMacroParameters0, macroTransformationContext0);
  }
}
