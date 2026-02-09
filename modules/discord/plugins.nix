{ ... }:
{
  enabled = [
    "FriendsSince"
    "NoTypingAnimation"
    "ReadAllNotificationsButton"
    "MessageLogger"
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
      showMode = 1;
    };

    RelationshipNotifier = {
      notices = true;
    };
  };
}
