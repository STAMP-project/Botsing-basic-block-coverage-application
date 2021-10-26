/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:44:47 UTC 2021
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
import org.xwiki.model.reference.DocumentReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XWikiAttachment_ESTest extends XWikiAttachment_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      XWikiDocument xWikiDocument0 = mock(XWikiDocument.class, new ViolatedAssumptionAnswer());
      doReturn((DocumentReference) null).when(xWikiDocument0).getDocumentReference();
      doReturn((String) null).when(xWikiDocument0).toString();
      XWikiAttachment xWikiAttachment0 = new XWikiAttachment(xWikiDocument0, "");
      xWikiAttachment0.getVersions();
      XWikiDocument xWikiDocument1 = mock(XWikiDocument.class, new ViolatedAssumptionAnswer());
      doReturn((DocumentReference) null).when(xWikiDocument1).getDocumentReference();
      XWikiAttachment xWikiAttachment1 = new XWikiAttachment(xWikiDocument1, "");
      // Undeclared exception!
      xWikiAttachment1.getReference();
  }
}
