import React, { useState } from "react";
import "./profile.css";
import axios from "axios";

export default function Profile() {

  const user =
    JSON.parse(
      localStorage.getItem("user")
    );

  const [name, setName] = useState(
    user?.name || ""
  );

  const [bio, setBio] = useState(
    user?.bio || ""
  );

  const [image, setImage] =
    useState(
      user?.image || ""
    );

  // ================= IMAGE CHANGE =================

  const handleImage = (e) => {

    const file = e.target.files[0];

    if (!file) return;

    const reader =
      new FileReader();

    reader.onloadend = () => {

      setImage(reader.result);

      const updatedUser = {

        ...user,

        image: reader.result,

        profilePic: reader.result

      };

      localStorage.setItem(
        "user",
        JSON.stringify(updatedUser)
      );

    };

    reader.readAsDataURL(file);

  };

  // ================= SAVE =================

  const saveProfile = async () => {

  try {

    const res = await axios.put(

      `http://localhost:5000/update-profile/${user._id}`,

      {
        name,
        bio,
        profilePic:image
      }

    );

    localStorage.setItem(

      "user",

      JSON.stringify(res.data)

    );

    alert("Profile Updated");

    window.location.reload();

  } catch(err){

    console.log(err);

  }

};

  return (

    <div className="profile-container">

      <div className="profile-card">

        {/* TOP COVER */}

        <div className="profile-top"></div>

        {/* PROFILE IMAGE */}

        <div className="profile-image-box">

          {image ? (

            <img
              src={image}
              alt=""
              className="profile-image"
            />

          ) : (

            <div className="default-avatar">

              {name
                ? name[0].toUpperCase()
                : "U"}

            </div>

          )}

          <label className="upload-btn">

            Change Photo

            <input
              type="file"
              hidden
              accept="image/*"
              onChange={handleImage}
            />

          </label>

        </div>

        {/* USER INFO */}

        <div className="profile-info">

          <h2>
            {name || "User"}
          </h2>

          <p className="email">
            {user?.email}
          </p>

        </div>

        {/* FORM */}

        <div className="profile-form">

          <label>
            Full Name
          </label>

          <input
            type="text"
            value={name}
            onChange={(e) =>
              setName(e.target.value)
            }
            placeholder="Enter your name"
          />

          <label>
            Bio
          </label>

          <textarea
            value={bio}
            onChange={(e) =>
              setBio(e.target.value)
            }
            placeholder="Write something..."
          />

          <label>
            Email
          </label>

          <input
            type="text"
            value={user?.email || ""}
            disabled
          />

          <button
            onClick={saveProfile}
            className="save-btn"
          >
            Save Profile
          </button>

        </div>

      </div>

    </div>

  );

}