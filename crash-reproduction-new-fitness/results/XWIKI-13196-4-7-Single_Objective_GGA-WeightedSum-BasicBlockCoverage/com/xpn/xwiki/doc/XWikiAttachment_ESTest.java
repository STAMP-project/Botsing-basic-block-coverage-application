/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:17:40 UTC 2020
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
      XWikiAttachment xWikiAttachment0 = new XWikiAttachment(xWikiDocument0, (String) null);
      xWikiAttachment0.setComment((String) null);
      XWikiDocument xWikiDocument1 = mock(XWikiDocument.class, new ViolatedAssumptionAnswer());
      doReturn((DocumentReference) null).when(xWikiDocument1).getDocumentReference();
      xWikiAttachment0.setDoc(xWikiDocument1);
      xWikiAttachment0.setComment("mimetype");
      // Undeclared exception!
      xWikiAttachment0.getReference();
  }
}
