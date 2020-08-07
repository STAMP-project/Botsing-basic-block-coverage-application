/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 13:21:56 UTC 2020
 */

package com.xpn.xwiki.doc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.doc.XWikiAttachment;
import com.xpn.xwiki.doc.XWikiDocument;
import com.xpn.xwiki.doc.merge.MergeConfiguration;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.model.reference.DocumentReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XWikiAttachment_ESTest extends XWikiAttachment_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      XWikiDocument xWikiDocument0 = mock(XWikiDocument.class, new ViolatedAssumptionAnswer());
      doReturn((DocumentReference) null).when(xWikiDocument0).getDocumentReference();
      XWikiAttachment xWikiAttachment0 = new XWikiAttachment(xWikiDocument0, "");
      xWikiAttachment0.setMetaDataDirty(false);
      xWikiAttachment0.getNextVersion();
      MergeConfiguration mergeConfiguration0 = mock(MergeConfiguration.class, new ViolatedAssumptionAnswer());
      xWikiAttachment0.getVersion();
      xWikiAttachment0.getRCSVersion();
      xWikiAttachment0.getDate();
      // Undeclared exception!
      xWikiAttachment0.getReference();
  }
}
