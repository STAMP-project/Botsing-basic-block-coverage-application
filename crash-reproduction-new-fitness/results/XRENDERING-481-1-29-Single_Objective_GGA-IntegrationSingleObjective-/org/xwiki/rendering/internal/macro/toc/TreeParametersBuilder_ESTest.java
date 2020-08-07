/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 07:15:18 UTC 2020
 */

package org.xwiki.rendering.internal.macro.toc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.lang.reflect.Type;
import java.util.Locale;
import java.util.Map;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.component.embed.EmbeddableComponentManager;
import org.xwiki.rendering.block.Block;
import org.xwiki.rendering.block.MacroBlock;
import org.xwiki.rendering.block.XDOM;
import org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder;
import org.xwiki.rendering.macro.toc.TocMacroParameters;
import org.xwiki.rendering.transformation.MacroTransformationContext;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class TreeParametersBuilder_ESTest extends TreeParametersBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Block block0 = mock(Block.class, new ViolatedAssumptionAnswer());
      MacroTransformationContext macroTransformationContext0 = new MacroTransformationContext();
      Class<Character> class0 = Character.TYPE;
      Locale locale0 = Locale.CANADA;
      TocMacroParameters tocMacroParameters0 = new TocMacroParameters();
      XDOM xDOM0 = XDOM.EMPTY;
      MacroTransformationContext macroTransformationContext1 = new MacroTransformationContext();
      TocMacroParameters.Scope tocMacroParameters_Scope0 = TocMacroParameters.Scope.LOCAL;
      tocMacroParameters0.setScope(tocMacroParameters_Scope0);
      MacroTransformationContext macroTransformationContext2 = new MacroTransformationContext();
      macroTransformationContext2.clone();
      TocMacroParameters tocMacroParameters1 = new TocMacroParameters();
      macroTransformationContext0.clone();
      EmbeddableComponentManager embeddableComponentManager0 = new EmbeddableComponentManager("");
      Map<String, String> map0 = embeddableComponentManager0.getInstanceMap((Type) class0);
      embeddableComponentManager0.getComponentDescriptor((Type) class0, ")q{__P(maX");
      MacroBlock macroBlock0 = new MacroBlock("", map0, "", false);
      MacroTransformationContext macroTransformationContext3 = new MacroTransformationContext();
      macroTransformationContext3.setCurrentMacroBlock(macroBlock0);
      TreeParametersBuilder treeParametersBuilder0 = new TreeParametersBuilder();
      treeParametersBuilder0.build(block0, tocMacroParameters1, macroTransformationContext3);
      // Undeclared exception!
      treeParametersBuilder0.build(macroBlock0, tocMacroParameters0, macroTransformationContext3);
  }
}
