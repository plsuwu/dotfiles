{ ... }:
{
  enabled = [
    "FriendsSince"
    "NoTypingAnimation"
    "ReadAllNotificationsButton"
    "ShowMeYourName"
    "NoReplyMention"
    "ShowHiddenChannels"
    "RelationshipNotifier"
    "ShowHiddenThings"
  ];

  settings = {
    ShowMeYourName = {
      mode = "nick-user";
      inReplies = true;
    };

    NoReplyMention = {
      inverseShiftReply = true;
    };

    ShowHiddenChannels = {
      showMode = 0;
    };

    RelationshipNotifier = {
      notices = true;
    };
  };
}
