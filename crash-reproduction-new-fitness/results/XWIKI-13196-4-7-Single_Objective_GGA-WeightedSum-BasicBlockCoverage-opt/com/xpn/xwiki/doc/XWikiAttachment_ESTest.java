/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:13:01 UTC 2021
 */

package com.xpn.xwiki.doc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.doc.XWikiAttachment;
import com.xpn.xwiki.doc.XWikiDocument;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.suigeneris.jrcs.rcs.Archive;
import org.xwiki.model.reference.DocumentReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XWikiAttachment_ESTest extends XWikiAttachment_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      XWikiDocument xWikiDocument0 = mock(XWikiDocument.class, new ViolatedAssumptionAnswer());
      doReturn((DocumentReference) null).when(xWikiDocument0).getDocumentReference();
      doReturn((String) null).when(xWikiDocument0).toString();
      String string0 = "";
      XWikiAttachment xWikiAttachment0 = new XWikiAttachment(xWikiDocument0, "");
      xWikiAttachment0.clone();
      xWikiAttachment0.setArchive((Archive) null);
      // Undeclared exception!
      xWikiAttachment0.getReference();
  }
}
