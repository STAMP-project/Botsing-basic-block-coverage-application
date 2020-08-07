/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 06:15:26 UTC 2020
 */

package org.xwiki.rendering.internal.macro.toc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.rendering.block.Block;
import org.xwiki.rendering.block.LinkBlock;
import org.xwiki.rendering.block.MacroBlock;
import org.xwiki.rendering.block.XDOM;
import org.xwiki.rendering.internal.macro.toc.TreeParameters;
import org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder;
import org.xwiki.rendering.listener.reference.ResourceReference;
import org.xwiki.rendering.listener.reference.ResourceType;
import org.xwiki.rendering.macro.toc.TocMacroParameters;
import org.xwiki.rendering.transformation.MacroTransformationContext;
import org.xwiki.rendering.util.IdGenerator;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class TreeParametersBuilder_ESTest extends TreeParametersBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ArrayList<Block> arrayList0 = new ArrayList<Block>();
      HashMap<String, String> hashMap0 = new HashMap<String, String>();
      TreeParametersBuilder treeParametersBuilder0 = new TreeParametersBuilder();
      MacroTransformationContext macroTransformationContext0 = new MacroTransformationContext();
      TocMacroParameters tocMacroParameters0 = new TocMacroParameters();
      HashMap<String, String> hashMap1 = new HashMap<String, String>();
      IdGenerator idGenerator0 = new IdGenerator();
      XDOM xDOM0 = new XDOM(arrayList0, idGenerator0);
      macroTransformationContext0.setXDOM(xDOM0);
      TreeParametersBuilder treeParametersBuilder1 = new TreeParametersBuilder();
      TreeParameters treeParameters0 = treeParametersBuilder1.build((Block) null, tocMacroParameters0, macroTransformationContext0);
      TreeParametersBuilder treeParametersBuilder2 = new TreeParametersBuilder();
      macroTransformationContext0.setId("4.4");
      TocMacroParameters.Scope tocMacroParameters_Scope0 = TocMacroParameters.Scope.LOCAL;
      MacroTransformationContext macroTransformationContext1 = macroTransformationContext0.clone();
      TreeParameters treeParameters1 = treeParametersBuilder1.build(treeParameters0.rootBlock, tocMacroParameters0, macroTransformationContext1);
      tocMacroParameters0.setScope(tocMacroParameters_Scope0);
      MacroTransformationContext macroTransformationContext2 = new MacroTransformationContext();
      TocMacroParameters tocMacroParameters1 = new TocMacroParameters();
      HashMap<String, String> hashMap2 = new HashMap<String, String>();
      TreeParametersBuilder treeParametersBuilder3 = new TreeParametersBuilder();
      treeParametersBuilder3.build(treeParameters1.rootBlock, tocMacroParameters1, macroTransformationContext0);
      TreeParametersBuilder treeParametersBuilder4 = new TreeParametersBuilder();
      macroTransformationContext1.setId((String) null);
      tocMacroParameters0.setScope(tocMacroParameters_Scope0);
      MacroTransformationContext macroTransformationContext3 = macroTransformationContext2.clone();
      ResourceType resourceType0 = ResourceType.UNC;
      Hashtable<String, String> hashtable0 = new Hashtable<String, String>();
      MacroBlock macroBlock0 = new MacroBlock((String) null, hashtable0, false);
      macroTransformationContext3.setCurrentMacroBlock(macroBlock0);
      ResourceReference resourceReference0 = new ResourceReference("", resourceType0);
      LinkBlock linkBlock0 = new LinkBlock(arrayList0, resourceReference0, false);
      // Undeclared exception!
      treeParametersBuilder3.build(treeParameters1.rootBlock, tocMacroParameters0, macroTransformationContext3);
  }
}
