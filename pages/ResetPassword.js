import axios from "axios";
import { useState } from "react";
import { useNavigate } from "react-router-dom";

export default function ResetPassword() {

  const [password, setPassword] =
    useState("");

  const nav = useNavigate();

  const email =
    localStorage.getItem("resetEmail");

  const resetPassword = async () => {
  try {
    await axios.post(
      "http://localhost:5000/auth/reset-password",
      {
        email,
        password
      }
    );

    nav("/success");
  } catch (err) {
    alert("Error");
  }
};

  return (

    <div className="auth-container">

      <div className="auth-box">

        <h2>Reset Password</h2>

        <input
          type="password"
          placeholder="New Password"
          onChange={(e) =>
            setPassword(e.target.value)
          }
        />

        <button onClick={resetPassword}>
          Reset Password
        </button>

      </div>

    </div>

  );

}