/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 13:30:06 UTC 2020
 */

package com.xpn.xwiki.doc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.XWikiContext;
import com.xpn.xwiki.doc.XWikiAttachment;
import com.xpn.xwiki.doc.XWikiDocument;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.model.reference.DocumentReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XWikiAttachment_ESTest extends XWikiAttachment_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      XWikiAttachment xWikiAttachment0 = new XWikiAttachment();
      xWikiAttachment0.clone();
      String string0 = null;
      DocumentReference documentReference0 = new DocumentReference("8>H+LuA^z3=kn", "s8P:X#:z", "8>H+LuA^z3=kn", (String) null);
      XWikiDocument xWikiDocument0 = new XWikiDocument(documentReference0);
      XWikiAttachment xWikiAttachment1 = new XWikiAttachment(xWikiDocument0, (String) null);
      // Undeclared exception!
      xWikiAttachment1.getMimeType((XWikiContext) null);
  }
}
