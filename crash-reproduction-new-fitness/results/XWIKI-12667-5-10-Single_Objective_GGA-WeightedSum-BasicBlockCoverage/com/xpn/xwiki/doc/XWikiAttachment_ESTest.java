/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 13:26:16 UTC 2020
 */

package com.xpn.xwiki.doc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.XWikiContext;
import com.xpn.xwiki.doc.XWikiAttachment;
import com.xpn.xwiki.doc.XWikiDocument;
import com.xpn.xwiki.internal.ReadOnlyXWikiContextProvider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.context.Execution;
import org.xwiki.context.ExecutionContext;
import org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver;
import org.xwiki.model.reference.DocumentReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XWikiAttachment_ESTest extends XWikiAttachment_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultStringEntityReferenceResolver defaultStringEntityReferenceResolver0 = new DefaultStringEntityReferenceResolver();
      XWikiDocument xWikiDocument0 = mock(XWikiDocument.class, new ViolatedAssumptionAnswer());
      doReturn((DocumentReference) null).when(xWikiDocument0).getDocumentReference();
      XWikiAttachment xWikiAttachment0 = new XWikiAttachment(xWikiDocument0, "");
      xWikiAttachment0.getFilename();
      xWikiAttachment0.getAttachment_content();
      ReadOnlyXWikiContextProvider readOnlyXWikiContextProvider0 = new ReadOnlyXWikiContextProvider();
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      doReturn((ExecutionContext) null).when(execution0).getContext();
      Injector.inject(readOnlyXWikiContextProvider0, (Class<?>) ReadOnlyXWikiContextProvider.class, "execution", (Object) execution0);
      Injector.validateBean(readOnlyXWikiContextProvider0, (Class<?>) ReadOnlyXWikiContextProvider.class);
      readOnlyXWikiContextProvider0.get();
      // Undeclared exception!
      xWikiAttachment0.isImage((XWikiContext) null);
  }
}
