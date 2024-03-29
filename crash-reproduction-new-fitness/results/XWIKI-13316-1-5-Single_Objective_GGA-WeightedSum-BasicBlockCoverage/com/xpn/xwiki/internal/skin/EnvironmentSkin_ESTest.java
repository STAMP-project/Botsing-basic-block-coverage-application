/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 16:37:54 UTC 2020
 */

package com.xpn.xwiki.internal.skin;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.XWikiContext;
import com.xpn.xwiki.internal.skin.EnvironmentSkin;
import com.xpn.xwiki.internal.skin.InternalSkinConfiguration;
import com.xpn.xwiki.internal.skin.InternalSkinManager;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.xwiki.environment.Environment;
import org.xwiki.rendering.syntax.SyntaxFactory;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EnvironmentSkin_ESTest extends EnvironmentSkin_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      InternalSkinManager internalSkinManager0 = mock(InternalSkinManager.class, new ViolatedAssumptionAnswer());
      InternalSkinConfiguration internalSkinConfiguration0 = mock(InternalSkinConfiguration.class, new ViolatedAssumptionAnswer());
      Logger logger0 = mock(Logger.class, new ViolatedAssumptionAnswer());
      SyntaxFactory syntaxFactory0 = mock(SyntaxFactory.class, new ViolatedAssumptionAnswer());
      Provider<XWikiContext> provider0 = (Provider<XWikiContext>) mock(Provider.class, new ViolatedAssumptionAnswer());
      InternalSkinManager internalSkinManager1 = mock(InternalSkinManager.class, new ViolatedAssumptionAnswer());
      InternalSkinConfiguration internalSkinConfiguration1 = mock(InternalSkinConfiguration.class, new ViolatedAssumptionAnswer());
      SyntaxFactory syntaxFactory1 = mock(SyntaxFactory.class, new ViolatedAssumptionAnswer());
      Provider<XWikiContext> provider1 = (Provider<XWikiContext>) mock(Provider.class, new ViolatedAssumptionAnswer());
      EnvironmentSkin environmentSkin0 = new EnvironmentSkin("_session", internalSkinManager1, internalSkinConfiguration0, logger0, syntaxFactory0, (Environment) null, provider0);
      environmentSkin0.createResource("/skins/MQ4?nksA940#IsB//skins/MQ4?nksA940#IsB/null", "_session");
      environmentSkin0.createResource((String) null, "_session");
      environmentSkin0.createResource((String) null, (String) null);
      // Undeclared exception!
      environmentSkin0.getOutputSyntaxString();
  }
}
