/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 17:09:10 UTC 2020
 */

package com.xpn.xwiki.doc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.XWikiContext;
import com.xpn.xwiki.doc.XWikiDocument;
import com.xpn.xwiki.web.EditForm;
import java.util.Locale;
import org.dom4j.Namespace;
import org.dom4j.tree.BaseElement;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.model.reference.DocumentReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XWikiDocument_ESTest extends XWikiDocument_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      String string0 = " \"";
      Namespace namespace0 = new Namespace(" \"", (String) null);
      BaseElement baseElement0 = new BaseElement((String) null, namespace0);
      DocumentReference documentReference0 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      doReturn((Locale) null).when(documentReference0).getLocale();
      XWikiDocument xWikiDocument0 = new XWikiDocument(documentReference0);
      EditForm editForm0 = mock(EditForm.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(editForm0).getComment();
      doReturn((String) null).when(editForm0).getCreator();
      doReturn((String) null).when(editForm0).getDefaultLanguage();
      doReturn((String) null).when(editForm0).getDefaultTemplate();
      doReturn((String) null).when(editForm0).getHidden();
      doReturn((String) null).when(editForm0).getParent();
      doReturn((String) null).when(editForm0).getSyntaxId();
      doReturn((String) null).when(editForm0).getTags();
      doReturn(false).when(editForm0).isMinorEdit();
      XWikiContext xWikiContext0 = new XWikiContext();
      xWikiDocument0.readDocMetaFromForm(editForm0, xWikiContext0);
      // Undeclared exception!
      xWikiDocument0.getTranslationList(xWikiContext0);
  }
}
