/**
 * Wire
 * Copyright (C) 2017 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.views

import android.content.Context
import android.support.v7.widget.{LinearLayoutManager, RecyclerView}
import android.util.AttributeSet
import android.view.View.OnClickListener
import android.view.{View, ViewGroup}
import com.waz.ZLog
import com.waz.ZLog._
import com.waz.model._
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, EventStream, Signal}
import com.waz.utils.returning
import com.waz.zclient.controllers.TeamsAndUserController
import com.waz.zclient.{Injectable, Injector, ViewHelper}

class TeamTabsView(val context: Context, val attrs: AttributeSet, val defStyleAttr: Int) extends RecyclerView(context, attrs, defStyleAttr) with ViewHelper {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null)

  val onTabClick = EventStream[Either[UserData, TeamData]]()

  setLayoutManager(new LinearLayoutManager(context, LinearLayoutManager.HORIZONTAL, false){
    override def canScrollHorizontally = true
    override def canScrollVertically = false
  })
  setOverScrollMode(View.OVER_SCROLL_NEVER)
  private val adapter = new TeamTabsAdapter(context)
  setAdapter(adapter)
}

class TeamTabViewHolder(view: TeamTabButton) extends RecyclerView.ViewHolder(view){
  def bind(data: Either[UserData, TeamData], selected: Boolean, unreadCount: Int): Unit = {
    view.setTag(data)
    data match {
      case Left(userData) => view.setUserData(userData, selected, unreadCount)
      case Right(teamData) => view.setTeamData(teamData, selected, unreadCount)
    }
  }
}

class TeamTabsAdapter(context: Context)(implicit injector: Injector, eventContext: EventContext) extends RecyclerView.Adapter[TeamTabViewHolder] with Injectable {
  private implicit val tag: LogTag = logTagFor[TeamTabsAdapter]

  val controller = inject[TeamsAndUserController]

  val onItemClick = EventStream[Either[UserData, TeamData]]()

  private var teams = Option.empty[Seq[(TeamData, Int)]]
  private var self = Option.empty[(UserData, Int)]

  onItemClick{ controller.currentTeamOrUser ! _ }

  Signal(controller.selfAndUnreadCount, controller.teamsAndUnreadCount, controller.currentTeamOrUser).on(Threading.Ui){
    case (cSelf, cTeams, _) =>
      self = Some(cSelf)
      teams = Some(cTeams.toSeq)
      notifyDataSetChanged()
  }

  override def getItemCount = 1 + controller.teams.currentValue.fold(0)(_.size)

  override def onBindViewHolder(holder: TeamTabViewHolder, position: Int) = {
    getItem(position) match {
      case Some(data) =>
        val userOrTeam = data match {
          case Left((userData, _)) => Left(userData)
          case Right((teamData, _)) => Right(teamData)
        }
        val count = data match {
          case Left((_, c)) => c
          case Right((_, c)) => c
          case _ => 0
        }
        val selected = (controller.currentTeamOrUser.currentValue, userOrTeam) match {
          case (Some(Left(currentUser)), Left(user)) => currentUser.id == user.id
          case (Some(Right(currentTeam)), Right(team)) => currentTeam.id == team.id
          case _ => false
        }
        holder.bind(userOrTeam, selected, count)
      case _ =>
        ZLog.error("Invalid get item index")
    }
  }

  override def onCreateViewHolder(parent: ViewGroup, viewType: Int) =
    new TeamTabViewHolder(returning(new TeamTabButton(context)) {
      _.setOnClickListener(new OnClickListener {
        override def onClick(v: View) = {
          onItemClick ! v.getTag.asInstanceOf[Either[UserData, TeamData]]
        }
      })
    })

  def getItem(position: Int): Option[Either[(UserData, Int), (TeamData, Int)]] = {
    position match {
      case 0 =>
        self.map(Left(_))
      case index =>
        teams.flatMap(_.lift(index - 1)).map(Right(_))
    }
  }
}
