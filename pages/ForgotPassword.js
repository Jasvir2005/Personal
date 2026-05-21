import axios from "axios";
import { useState } from "react";
import { useNavigate } from "react-router-dom";

export default function ForgotPassword() {

  const [email, setEmail] =
    useState("");

  const nav = useNavigate();

  const sendOtp = async () => {

    try {

      await axios.post(
        "http://localhost:5000/auth/forgot-password",
        { email }
      );

      alert("OTP Sent");

      localStorage.setItem(
        "resetEmail",
        email
      );

      nav("/verify-otp");

    } catch (err) {

      alert("Error");

    }

  };

  return (

    <div className="auth-container">

      <div className="auth-box">

        <h2>Forgot Password</h2>

        <input
          type="email"
          placeholder="Enter Email"
          onChange={(e) =>
            setEmail(e.target.value)
          }
        />

        <button onClick={sendOtp}>
          Send OTP
        </button>

      </div>

    </div>

  );

}