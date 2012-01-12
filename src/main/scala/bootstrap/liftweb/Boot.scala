package bootstrap.liftweb

import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import jr.model._
import jr.spec.Spec
import jr.rest.autocomplete.ServiceableRest
import provider.HTTPRequest

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("jr")

    LiftRules.early.append(makeUtf8)
    LiftRules.statelessDispatchTable.append(ServiceableRest)

    // Build SiteMap
    LiftRules.setSiteMap(SiteMap(Spec.entries: _*))

    Model.init()
  }

  private

  def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }


}

