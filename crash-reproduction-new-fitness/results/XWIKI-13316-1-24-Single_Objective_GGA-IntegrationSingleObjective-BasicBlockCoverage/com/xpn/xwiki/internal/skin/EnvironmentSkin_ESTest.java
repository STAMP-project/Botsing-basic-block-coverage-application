/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 21:44:38 UTC 2020
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
      Provider<XWikiContext> provider0 = (Provider<XWikiContext>) mock(Provider.class, new ViolatedAssumptionAnswer());
      EnvironmentSkin environmentSkin0 = new EnvironmentSkin("#Hlk`EqN", internalSkinManager0, internalSkinConfiguration0, logger0, (SyntaxFactory) null, (Environment) null, provider0);
      environmentSkin0.getSkinFolder();
      environmentSkin0.getSkinFolder();
      // Undeclared exception!
      environmentSkin0.getOutputSyntaxString();
  }
}
